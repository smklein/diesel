use crate::dsl::{Filter, IntoBoxed, OrFilter};
use crate::expression::{AppearsInQuery, SelectableExpression};
use crate::query_builder::returning_clause::*;
use crate::query_builder::where_clause::*;
use crate::query_builder::*;
use crate::query_dsl::methods::{BoxedDsl, FilterDsl, OrFilterDsl};
use crate::query_dsl::RunQueryDsl;
use crate::query_source::{QuerySource, Table};
use crate::result::QueryResult;

use super::from_clause::FromClause;

#[must_use = "Queries are only executed when calling `load`, `get_result` or similar."]
/// Represents a SQL `DELETE` statement.
///
/// The type parameters on this struct represent:
///
/// - `T`: The table we are deleting from.
/// - `Ret`: The `RETURNING` clause of this query. The exact types used to
///   represent this are private. You can safely rely on the default type
///   representing the lack of a `RETURNING` clause.
pub struct DeleteStatement<T: QuerySource, Ret = NoReturningClause> {
    from_clause: FromClause<T>,
    where_clause: BoxedWhereClause<'static>,
    returning: Ret,
}

impl<T, Ret> std::fmt::Debug for DeleteStatement<T, Ret>
where
    T: QuerySource,
    FromClause<T>: std::fmt::Debug,
    Ret: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DeleteStatement")
            .field("from_clause", &self.from_clause)
            .field("returning", &self.returning)
            .finish()
    }
}

impl<T, Ret> QueryId for DeleteStatement<T, Ret>
where
    T: QuerySource + QueryId + 'static,
    Ret: QueryId,
{
    type QueryId = ();

    const HAS_STATIC_QUERY_ID: bool = false;
}

type BoxedDeleteStatement<T, Ret = NoReturningClause> =
    DeleteStatement<T, Ret>;

impl<T: QuerySource> DeleteStatement<T, NoReturningClause> {
    pub(crate) fn new(table: T, where_clause: BoxedWhereClause<'static>) -> Self {
        DeleteStatement {
            from_clause: FromClause::new(table),
            where_clause,
            returning: NoReturningClause,
        }
    }

    /// Adds the given predicate to the `WHERE` clause of the statement being
    /// constructed.
    ///
    /// If there is already a `WHERE` clause, the predicate will be appended
    /// with `AND`. There is no difference in behavior between
    /// `delete(table.filter(x))` and `delete(table).filter(x)`.
    ///
    /// # Example
    ///
    /// ```rust
    /// # include!("../../doctest_setup.rs");
    /// #
    /// # fn main() {
    /// #     use schema::users::dsl::*;
    /// #     let connection = &mut establish_connection();
    /// let deleted_rows = diesel::delete(users)
    ///     .filter(name.eq("Sean"))
    ///     .execute(connection);
    /// assert_eq!(Ok(1), deleted_rows);
    ///
    /// let expected_names = vec!["Tess".to_string()];
    /// let names = users.select(name).load(connection);
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

    /// Adds to the `WHERE` clause of a query using `OR`
    ///
    /// If there is already a `WHERE` clause, the result will be `(old OR new)`.
    /// Calling `foo.filter(bar).or_filter(baz)`
    /// is identical to `foo.filter(bar.or(baz))`.
    /// However, the second form is much harder to do dynamically.
    ///
    /// # Example
    ///
    /// ```rust
    /// # include!("../../doctest_setup.rs");
    /// #
    /// # fn main() {
    /// #     use schema::users::dsl::*;
    /// #     let connection = &mut establish_connection();
    /// let deleted_rows = diesel::delete(users)
    ///     .filter(name.eq("Sean"))
    ///     .or_filter(name.eq("Tess"))
    ///     .execute(connection);
    /// assert_eq!(Ok(2), deleted_rows);
    ///
    /// let num_users = users.count().first(connection);
    ///
    /// assert_eq!(Ok(0), num_users);
    /// # }
    /// ```
    pub fn or_filter<Predicate>(self, predicate: Predicate) -> OrFilter<Self, Predicate>
    where
        Self: OrFilterDsl<Predicate>,
    {
        OrFilterDsl::or_filter(self, predicate)
    }

    /// Boxes the `WHERE` clause of this delete statement.
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
    /// #     params.insert("sean_has_been_a_jerk", true);
    /// let mut query = diesel::delete(users)
    ///     .into_boxed();
    ///
    /// if params["sean_has_been_a_jerk"] {
    ///     query = query.filter(name.eq("Sean"));
    /// }
    ///
    /// let deleted_rows = query.execute(connection)?;
    /// assert_eq!(1, deleted_rows);
    ///
    /// let expected_names = vec!["Tess"];
    /// let names = users.select(name).load::<String>(connection)?;
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

impl<T, Ret, Predicate> FilterDsl<Predicate> for DeleteStatement<T, Ret>
where
    BoxedWhereClause<'static>: WhereAnd<Predicate, Output = BoxedWhereClause<'static>>,
    Predicate: AppearsInQuery<T>,
    T: QuerySource,
{
    type Output = Self;

    fn filter(self, predicate: Predicate) -> Self::Output {
        DeleteStatement {
            from_clause: self.from_clause,
            where_clause: self.where_clause.and(predicate),
            returning: self.returning,
        }
    }
}

impl<T, Ret, Predicate> OrFilterDsl<Predicate> for DeleteStatement<T, Ret>
where
    T: QuerySource,
    BoxedWhereClause<'static>: WhereOr<Predicate, Output = BoxedWhereClause<'static>>,
    Predicate: AppearsInQuery<T>,
{
    type Output = Self;

    fn or_filter(self, predicate: Predicate) -> Self::Output {
        DeleteStatement {
            from_clause: self.from_clause,
            where_clause: self.where_clause.or(predicate),
            returning: self.returning,
        }
    }
}

impl<'a, T, Ret> BoxedDsl<'a> for DeleteStatement<T, Ret>
where
    T: QuerySource,
{
    type Output = BoxedDeleteStatement<T, Ret>;

    fn internal_into_boxed(self) -> Self::Output {
        DeleteStatement {
            where_clause: self.where_clause,
            returning: self.returning,
            from_clause: self.from_clause,
        }
    }
}

impl<T, Ret> QueryFragment for DeleteStatement<T, Ret>
where
    T: Table,
    FromClause<T>: QueryFragment,
    Ret: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_sql("DELETE ");
        self.from_clause.walk_ast(out.reborrow())?;
        self.where_clause.walk_ast(out.reborrow())?;
        self.returning.walk_ast(out.reborrow())?;
        Ok(())
    }
}

impl<T> AsQuery for DeleteStatement<T, NoReturningClause>
where
    T: Table,
    T::AllColumns: SelectableExpression<T>,
    DeleteStatement<T, ReturningClause<T::AllColumns>>: Query,
{
    type SqlType = <Self::Query as Query>::SqlType;
    type Query = DeleteStatement<T, ReturningClause<T::AllColumns>>;

    fn as_query(self) -> Self::Query {
        self.returning(T::all_columns())
    }
}

impl<T, Ret> Query for DeleteStatement<T, ReturningClause<Ret>>
where
    T: Table,
    Ret: SelectableExpression<T>,
{
    type SqlType = Ret::SqlType;
}

impl<T, Ret, Conn> RunQueryDsl<Conn> for DeleteStatement<T, Ret> where T: QuerySource {}

impl<T: QuerySource> DeleteStatement<T, NoReturningClause> {
    /// Specify what expression is returned after execution of the `delete`.
    ///
    /// # Examples
    ///
    /// ### Deleting a record:
    ///
    /// ```rust
    /// # include!("../../doctest_setup.rs");
    /// #
    /// # #[cfg(feature = "postgres")]
    /// # fn main() {
    /// #     use schema::users::dsl::*;
    /// #     let connection = &mut establish_connection();
    /// let deleted_name = diesel::delete(users.filter(name.eq("Sean")))
    ///     .returning(name)
    ///     .get_result(connection);
    /// assert_eq!(Ok("Sean".to_string()), deleted_name);
    /// # }
    /// # #[cfg(not(feature = "postgres"))]
    /// # fn main() {}
    /// ```
    pub fn returning<E>(self, returns: E) -> DeleteStatement<T, ReturningClause<E>>
    where
        E: SelectableExpression<T>,
        DeleteStatement<T, ReturningClause<E>>: Query,
    {
        DeleteStatement {
            where_clause: self.where_clause,
            from_clause: self.from_clause,
            returning: ReturningClause(returns),
        }
    }
}
