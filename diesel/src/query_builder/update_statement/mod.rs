pub(crate) mod changeset;
pub(super) mod target;

use self::target::UpdateTarget;

use crate::dsl::{Filter, IntoBoxed};
use crate::expression::{
    AppearsInQuery, Expression, SelectableExpression,
};
use crate::query_builder::returning_clause::*;
use crate::query_builder::where_clause::*;
use crate::query_dsl::methods::{BoxedDsl, FilterDsl};
use crate::query_dsl::RunQueryDsl;
use crate::query_source::Table;
use crate::result::Error::QueryBuilderError;
use crate::result::QueryResult;
use crate::{query_builder::*, QuerySource};

impl<T: QuerySource> UpdateStatement<T, SetNotCalled> {
    pub(crate) fn new(target: UpdateTarget<T>) -> Self {
        UpdateStatement {
            from_clause: target.table.from_clause(),
            where_clause: target.where_clause,
            values: SetNotCalled,
            returning: NoReturningClause,
        }
    }

    /// Provides the `SET` clause of the `UPDATE` statement.
    ///
    /// See [`update`](crate::update()) for usage examples, or [the update
    /// guide](https://diesel.rs/guides/all-about-updates/) for a more exhaustive
    /// set of examples.
    pub fn set<V>(self, values: V) -> UpdateStatement<T, V::Changeset>
    where
        T: Table,
        V: changeset::AsChangeset<Target = T>,
        UpdateStatement<T, V::Changeset>: AsQuery,
    {
        UpdateStatement {
            from_clause: self.from_clause,
            where_clause: self.where_clause,
            values: values.as_changeset(),
            returning: self.returning,
        }
    }
}

#[must_use = "Queries are only executed when calling `load`, `get_result` or similar."]
/// Represents a complete `UPDATE` statement.
///
/// See [`update`](crate::update()) for usage examples, or [the update
/// guide](https://diesel.rs/guides/all-about-updates/) for a more exhaustive
/// set of examples.
pub struct UpdateStatement<T: QuerySource, V = SetNotCalled, Ret = NoReturningClause> {
    from_clause: T::FromClause,
    where_clause: BoxedWhereClause<'static>,
    values: V,
    returning: Ret,
}

/// An `UPDATE` statement with a boxed `WHERE` clause.
pub type BoxedUpdateStatement<T, V = SetNotCalled, Ret = NoReturningClause> =
    UpdateStatement<T, V, Ret>;

impl<T: QuerySource, V, Ret> UpdateStatement<T, V, Ret> {
    /// Adds the given predicate to the `WHERE` clause of the statement being
    /// constructed.
    ///
    /// If there is already a `WHERE` clause, the predicate will be appended
    /// with `AND`. There is no difference in behavior between
    /// `update(table.filter(x))` and `update(table).filter(x)`.
    ///
    /// # Example
    ///
    /// ```rust
    /// # include!("../../doctest_setup.rs");
    /// #
    /// # fn main() {
    /// #     use schema::users::dsl::*;
    /// #     let connection = &mut establish_connection();
    /// let updated_rows = diesel::update(users)
    ///     .set(name.eq("Jim"))
    ///     .filter(name.eq("Sean"))
    ///     .execute(connection);
    /// assert_eq!(Ok(1), updated_rows);
    ///
    /// let expected_names = vec!["Jim".to_string(), "Tess".to_string()];
    /// let names = users.select(name).order(id).load(connection);
    ///
    /// assert_eq!(Ok(expected_names), names);
    /// # }
    /// ```
    pub fn filter<Predicate>(self, predicate: Predicate) -> Filter<Self, Predicate>
    where
        Self: FilterDsl<Predicate>,
    {
        FilterDsl::filter(self, predicate)
    }

    /// Boxes the `WHERE` clause of this update statement.
    ///
    /// This is useful for cases where you want to conditionally modify a query,
    /// but need the type to remain the same. The backend must be specified as
    /// part of this. It is not possible to box a query and have it be useable
    /// on multiple backends.
    ///
    /// A boxed query will incur a minor performance penalty, as the query builder
    /// can no longer be inlined by the compiler. For most applications this cost
    /// will be minimal.
    ///
    /// ### Example
    ///
    /// ```rust
    /// # include!("../../doctest_setup.rs");
    /// #
    /// # fn main() {
    /// #     run_test().unwrap();
    /// # }
    /// #
    /// # fn run_test() -> QueryResult<()> {
    /// #     use std::collections::HashMap;
    /// #     use schema::users::dsl::*;
    /// #     let connection = &mut establish_connection();
    /// #     let mut params = HashMap::new();
    /// #     params.insert("tess_has_been_a_jerk", false);
    /// let mut query = diesel::update(users)
    ///     .set(name.eq("Jerk"))
    ///     .into_boxed();
    ///
    /// if !params["tess_has_been_a_jerk"] {
    ///     query = query.filter(name.ne("Tess"));
    /// }
    ///
    /// let updated_rows = query.execute(connection)?;
    /// assert_eq!(1, updated_rows);
    ///
    /// let expected_names = vec!["Jerk", "Tess"];
    /// let names = users.select(name).order(id).load::<String>(connection)?;
    ///
    /// assert_eq!(expected_names, names);
    /// #     Ok(())
    /// # }
    /// ```
    pub fn into_boxed<'a>(self) -> IntoBoxed<'a, Self>
    where
        Self: BoxedDsl<'a>,
    {
        BoxedDsl::internal_into_boxed(self)
    }
}

impl<T, V, Ret, Predicate> FilterDsl<Predicate> for UpdateStatement<T, V, Ret>
where
    T: QuerySource,
    BoxedWhereClause<'static>: WhereAnd<Predicate, Output = BoxedWhereClause<'static>>,
    Predicate: AppearsInQuery<T>,
{
    type Output = UpdateStatement<T, V, Ret>;

    fn filter(self, predicate: Predicate) -> Self::Output {
        UpdateStatement {
            from_clause: self.from_clause,
            where_clause: self.where_clause.and(predicate),
            values: self.values,
            returning: self.returning,
        }
    }
}

impl<'a, T, V, Ret> BoxedDsl<'a> for UpdateStatement<T, V, Ret>
where
    T: QuerySource,
{
    type Output = BoxedUpdateStatement<T, V, Ret>;

    fn internal_into_boxed(self) -> Self::Output {
        UpdateStatement {
            from_clause: self.from_clause,
            where_clause: self.where_clause,
            values: self.values,
            returning: self.returning,
        }
    }
}

impl<T, V, Ret> QueryFragment for UpdateStatement<T, V, Ret>
where
    T: Table,
    T::FromClause: QueryFragment,
    V: QueryFragment,
    Ret: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        if self.values.is_noop(out.backend())? {
            return Err(QueryBuilderError(
                "There are no changes to save. This query cannot be built".into(),
            ));
        }

        out.unsafe_to_cache_prepared();
        out.push_sql("UPDATE ");
        self.from_clause.walk_ast(out.reborrow())?;
        out.push_sql(" SET ");
        self.values.walk_ast(out.reborrow())?;
        self.where_clause.walk_ast(out.reborrow())?;
        self.returning.walk_ast(out.reborrow())?;
        Ok(())
    }
}

impl<T, V, Ret> QueryId for UpdateStatement<T, V, Ret>
where
    T: QuerySource,
{
    type QueryId = ();

    const HAS_STATIC_QUERY_ID: bool = false;
}

impl<T, V> AsQuery for UpdateStatement<T, V, NoReturningClause>
where
    T: Table,
    UpdateStatement<T, V, ReturningClause<T::AllColumns>>: Query,
{
    type SqlType = <Self::Query as Query>::SqlType;
    type Query = UpdateStatement<T, V, ReturningClause<T::AllColumns>>;

    fn as_query(self) -> Self::Query {
        self.returning(T::all_columns())
    }
}

impl<T, V, Ret> Query for UpdateStatement<T, V, ReturningClause<Ret>>
where
    T: Table,
    Ret: Expression + SelectableExpression<T>,
{
    type SqlType = Ret::SqlType;
}

impl<T: QuerySource, V, Ret, Conn> RunQueryDsl<Conn> for UpdateStatement<T, V, Ret> {}

impl<T: QuerySource, V> UpdateStatement<T, V, NoReturningClause> {
    /// Specify what expression is returned after execution of the `update`.
    /// # Examples
    ///
    /// ### Updating a single record:
    ///
    /// ```rust
    /// # include!("../../doctest_setup.rs");
    /// #
    /// # #[cfg(feature = "postgres")]
    /// # fn main() {
    /// #     use schema::users::dsl::*;
    /// #     let connection = &mut establish_connection();
    /// let updated_name = diesel::update(users.filter(id.eq(1)))
    ///     .set(name.eq("Dean"))
    ///     .returning(name)
    ///     .get_result(connection);
    /// assert_eq!(Ok("Dean".to_string()), updated_name);
    /// # }
    /// # #[cfg(not(feature = "postgres"))]
    /// # fn main() {}
    /// ```
    pub fn returning<E>(self, returns: E) -> UpdateStatement<T, V, ReturningClause<E>>
    where
        T: Table,
        UpdateStatement<T, V, ReturningClause<E>>: Query,
    {
        UpdateStatement {
            from_clause: self.from_clause,
            where_clause: self.where_clause,
            values: self.values,
            returning: ReturningClause(returns),
        }
    }
}

/// Indicates that you have not yet called `.set` on an update statement
#[derive(Debug, Clone, Copy)]
pub struct SetNotCalled;
