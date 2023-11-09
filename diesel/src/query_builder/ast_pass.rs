use std::fmt;

use crate::backend::Backend;
use crate::query_builder::{BindCollector, DB, QueryBuilder};
use crate::result::QueryResult;
use crate::serialize::ToSql;
use crate::sql_types::{HasSqlType, TypeMetadata};

type DbQueryBuilder = <DB as Backend>::QueryBuilder;
type DbBindCollector<'a> = <DB as Backend>::BindCollector<'a>;
type DbMetadataLookup = <DB as TypeMetadata>::MetadataLookup;

#[allow(missing_debug_implementations)]
/// The primary type used when walking a Diesel AST during query execution.
///
/// Executing a query is generally done in multiple passes. This list includes,
/// but is not limited to:
///
/// - Generating the SQL
/// - Collecting and serializing bound values (sent separately from the SQL)
/// - Determining if a query is safe to store in the prepared statement cache
///
/// When adding a new type that is used in a Diesel AST, you don't need to care
/// about which specific passes are being performed, nor is there any way for
/// you to find out what the current pass is. You should simply call the
/// relevant methods and trust that they will be a no-op if they're not relevant
/// to the current pass.
pub struct AstPass<'a, 'b>
where
    'b: 'a,
    <DB as TypeMetadata>::MetadataLookup: 'a,
{
    internals: AstPassInternals<'a, 'b>,
    backend: &'b DB,
}

impl<'a, 'b> AstPass<'a, 'b>
where
    'b: 'a,
{
    pub(crate) fn to_sql(
        query_builder: &'a mut DbQueryBuilder,
        options: &'a mut AstPassToSqlOptions,
        backend: &'b DB,
    ) -> Self {
        AstPass {
            internals: AstPassInternals::ToSql(query_builder, options),
            backend,
        }
    }

    pub(crate) fn collect_binds(
        collector: &'a mut DbBindCollector<'b>,
        metadata_lookup: &'a mut DbMetadataLookup,
        backend: &'b DB,
    ) -> Self {
        AstPass {
            internals: AstPassInternals::CollectBinds {
                collector,
                metadata_lookup,
            },
            backend,
        }
    }

    pub(crate) fn is_safe_to_cache_prepared(result: &'a mut bool, backend: &'b DB) -> Self {
        AstPass {
            internals: AstPassInternals::IsSafeToCachePrepared(result),
            backend,
        }
    }

    pub(crate) fn debug_binds(
        formatter: &'a mut Vec<Box<dyn fmt::Debug + 'b>>,
        backend: &'b DB,
    ) -> Self {
        AstPass {
            internals: AstPassInternals::DebugBinds(formatter),
            backend,
        }
    }

    /// Does running this AST pass have any effect?
    ///
    /// The result will be set to `false` if any method that generates SQL
    /// is called.
    pub(crate) fn is_noop(result: &'a mut bool, backend: &'b DB) -> Self {
        AstPass {
            internals: AstPassInternals::IsNoop(result),
            backend,
        }
    }

    #[cfg(feature = "sqlite")]
    pub(crate) fn skip_from(&mut self, value: bool) {
        if let AstPassInternals::ToSql(_, ref mut options) = self.internals {
            options.skip_from = value
        }
    }

    /// Call this method whenever you pass an instance of `AstPass` by value.
    ///
    /// Effectively copies `self`, with a narrower lifetime. When passing a
    /// reference or a mutable reference, this is normally done by rust
    /// implicitly. This is why you can pass `&mut Foo` to multiple functions,
    /// even though mutable references are not `Copy`. However, this is only
    /// done implicitly for references. For structs with lifetimes it must be
    /// done explicitly. This method matches the semantics of what Rust would do
    /// implicitly if you were passing a mutable reference
    #[allow(clippy::explicit_auto_deref)] // clippy is wrong here
    pub fn reborrow(&'_ mut self) -> AstPass<'_, 'b> {
        let internals = match self.internals {
            AstPassInternals::ToSql(ref mut builder, ref mut options) => {
                AstPassInternals::ToSql(*builder, options)
            }
            AstPassInternals::CollectBinds {
                ref mut collector,
                ref mut metadata_lookup,
            } => AstPassInternals::CollectBinds {
                collector: *collector,
                metadata_lookup: *metadata_lookup,
            },
            AstPassInternals::IsSafeToCachePrepared(ref mut result) => {
                AstPassInternals::IsSafeToCachePrepared(result)
            }
            AstPassInternals::DebugBinds(ref mut f) => AstPassInternals::DebugBinds(f),
            AstPassInternals::IsNoop(ref mut result) => AstPassInternals::IsNoop(result),
        };
        AstPass {
            internals,
            backend: self.backend,
        }
    }

    /// Mark the current query being constructed as unsafe to store in the
    /// prepared statement cache.
    ///
    /// Diesel caches prepared statements as much as possible. However, it is
    /// important to ensure that this doesn't result in unbounded memory usage
    /// on the database server. To ensure this is the case, any logical query
    /// which could generate a potentially unbounded number of prepared
    /// statements *must* call this method. Examples of AST nodes which do this
    /// are:
    ///
    /// - `SqlLiteral`. We have no way of knowing if the SQL string was
    ///   constructed dynamically or not, so we must assume it was dynamic.
    /// - `EqAny` when passed a Rust `Vec`. The `IN` operator requires one bind
    ///   parameter per element, meaning that the query could generate up to
    ///   `usize` unique prepared statements.
    /// - `InsertStatement`. Unbounded due to the variable number of records
    ///   being inserted generating unique SQL.
    /// - `UpdateStatement`. The number of potential queries is actually
    ///   technically bounded, but the upper bound is the number of columns on
    ///   the table factorial which is too large to be safe.
    pub fn unsafe_to_cache_prepared(&mut self) {
        if let AstPassInternals::IsSafeToCachePrepared(ref mut result) = self.internals {
            **result = false
        }
    }

    /// Push the given SQL string on the end of the query being constructed.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use diesel::query_builder::{QueryFragment, AstPass};
    /// # use diesel::backend::Backend;
    /// # use diesel::QueryResult;
    /// # struct And<Left, Right> { left: Left, right: Right }
    /// impl<Left, Right> QueryFragment for And<Left, Right>
    /// where
    ///     Left: QueryFragment,
    ///     Right: QueryFragment,
    /// {
    ///     fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
    ///         self.left.walk_ast(out.reborrow())?;
    ///         out.push_sql(" AND ");
    ///         self.right.walk_ast(out.reborrow())?;
    ///         Ok(())
    ///     }
    /// }
    /// # fn main() {}
    /// ```
    pub fn push_sql(&mut self, sql: &str) {
        match self.internals {
            AstPassInternals::ToSql(ref mut builder, _) => builder.push_sql(sql),
            AstPassInternals::IsNoop(ref mut result) => **result = false,
            _ => {}
        }
    }

    /// Push the given SQL identifier on the end of the query being constructed.
    ///
    /// The identifier will be quoted using the rules specific to the backend
    /// the query is being constructed for.
    pub fn push_identifier(&mut self, identifier: &str) -> QueryResult<()> {
        match self.internals {
            AstPassInternals::ToSql(ref mut builder, _) => builder.push_identifier(identifier)?,
            AstPassInternals::IsNoop(ref mut result) => **result = false,
            _ => {}
        }
        Ok(())
    }

    /// Push a value onto the given query to be sent separate from the SQL
    ///
    /// This method affects multiple AST passes. It should be called at the
    /// point in the query where you'd want the parameter placeholder (`$1` on
    /// PG, `?` on other backends) to be inserted.
    pub fn push_bind_param<T, U>(&mut self, bind: &'b U) -> QueryResult<()>
    where
        DB: HasSqlType<T>,
        U: ToSql<T> + ?Sized,
    {
        match self.internals {
            AstPassInternals::ToSql(ref mut out, _) => out.push_bind_param(),
            AstPassInternals::CollectBinds {
                ref mut collector,
                ref mut metadata_lookup,
            } => collector.push_bound_value(bind, metadata_lookup)?,
            AstPassInternals::DebugBinds(ref mut f) => {
                f.push(Box::new(bind));
            }
            AstPassInternals::IsNoop(ref mut result) => **result = false,
            _ => {}
        }
        Ok(())
    }

    /// Push a value onto the given query to be sent separate from the SQL
    ///
    /// This method affects multiple AST passes. It should be called at the
    /// point in the raw SQL is inserted. This assumes the parameter placeholder
    /// (`$1` on PG, `?` on other backends) is already inserted.
    #[diesel_derives::__diesel_public_if(
        feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes"
    )]
    pub(crate) fn push_bind_param_value_only<T, U>(&mut self, bind: &'b U) -> QueryResult<()>
    where
        DB: HasSqlType<T>,
        U: ToSql<T> + ?Sized,
    {
        match self.internals {
            AstPassInternals::CollectBinds { .. } | AstPassInternals::DebugBinds(..) => {
                self.push_bind_param(bind)?
            }
            AstPassInternals::ToSql(ref mut out, _) => {
                out.push_bind_param_value_only();
            }
            _ => {}
        }
        Ok(())
    }

    /// Get information about the backend that will consume this query
    #[cfg_attr(
        not(feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes"),
        doc(hidden)
    )] // This is used by the `sql_function` macro
    #[cfg_attr(
        doc_cfg,
        doc(cfg(feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes"))
    )]
    pub fn backend(&self) -> &DB {
        self.backend
    }

    /// Get if the query should be rendered with from clauses or not
    #[cfg_attr(
        not(feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes"),
        doc(hidden)
    )] // This is used by the `__diesel_column` macro
    #[cfg_attr(
        doc_cfg,
        doc(cfg(feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes"))
    )]
    pub fn should_skip_from(&self) -> bool {
        if let AstPassInternals::ToSql(_, ref options) = self.internals {
            options.skip_from
        } else {
            false
        }
    }
}

#[allow(missing_debug_implementations)]
/// This is separate from the struct to cause the enum to be opaque, forcing
/// usage of the methods provided rather than matching on the enum directly.
/// This essentially mimics the capabilities that would be available if
/// `AstPass` were a trait.
enum AstPassInternals<'a, 'b>
where
    'b: 'a,
    DbQueryBuilder: 'a,
    DbMetadataLookup: 'a,
{
    ToSql(&'a mut DbQueryBuilder, &'a mut AstPassToSqlOptions),
    CollectBinds {
        collector: &'a mut DbBindCollector<'b>,
        metadata_lookup: &'a mut DbMetadataLookup,
    },
    IsSafeToCachePrepared(&'a mut bool),
    DebugBinds(&'a mut Vec<Box<dyn fmt::Debug + 'b>>),
    IsNoop(&'a mut bool),
}

#[diesel_derives::__diesel_public_if(
    feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes"
)]
#[allow(missing_debug_implementations)]
#[allow(missing_copy_implementations)]
#[derive(Default)]
/// This is used to pass down additional settings to the `AstPass`
/// when rendering the sql string.
pub(crate) struct AstPassToSqlOptions {
    skip_from: bool,
}
