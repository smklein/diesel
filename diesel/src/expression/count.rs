use std::marker::PhantomData;

use super::functions::sql_function;
use super::AsExpression;
use super::Expression;
use crate::query_builder::*;
use crate::result::QueryResult;
use crate::sql_types::{BigInt, DieselNumericOps, SingleValue, SqlType};
use crate::{AppearsInQuery, SelectableExpression};

sql_function! {
    /// Creates a SQL `COUNT` expression
    ///
    /// As with most bare functions, this is not exported by default. You can import
    /// it specifically as `diesel::dsl::count`, or glob import
    /// `diesel::dsl::*`
    ///
    /// # Examples
    ///
    /// ```rust
    /// # include!("../doctest_setup.rs");
    /// # use diesel::dsl::*;
    /// #
    /// # fn main() {
    /// #     use schema::animals::dsl::*;
    /// #     let connection = &mut establish_connection();
    /// assert_eq!(Ok(1), animals.select(count(name)).first(connection));
    /// # }
    /// ```
    #[aggregate]
    fn count<T: SqlType + SingleValue>(expr: T) -> BigInt;
}

/// Creates a SQL `COUNT(*)` expression
///
/// For selecting the count of a query, and nothing else, you can just call
/// [`count`](crate::query_dsl::QueryDsl::count())
/// on the query instead.
///
/// As with most bare functions, this is not exported by default. You can import
/// it specifically as `diesel::dsl::count_star`, or glob import
/// `diesel::dsl::*`
///
/// # Examples
///
/// ```rust
/// # include!("../doctest_setup.rs");
/// # use diesel::dsl::*;
/// #
/// # fn main() {
/// #     use schema::users::dsl::*;
/// #     let connection = &mut establish_connection();
/// assert_eq!(Ok(2), users.select(count_star()).first(connection));
/// # }
/// ```
pub fn count_star() -> CountStar {
    CountStar
}

#[derive(Debug, Clone, Copy, QueryId, DieselNumericOps)]
#[doc(hidden)]
pub struct CountStar;

impl Expression for CountStar {
    type SqlType = BigInt;
}

impl QueryFragment for CountStar {
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_sql("COUNT(*)");
        Ok(())
    }
}

impl_selectable_expression!(CountStar);

/// Creates a SQL `COUNT(DISTINCT ...)` expression
///
/// As with most bare functions, this is not exported by default. You can import
/// it specifically as `diesel::dsl::count_distinct`, or glob import
/// `diesel::dsl::*`
///
/// # Examples
///
/// ```rust
/// # #[macro_use] extern crate diesel;
/// # include!("../doctest_setup.rs");
/// # use diesel::dsl::*;
/// #
/// # fn main() {
/// #     use schema::posts::dsl::*;
/// #     let connection = &mut establish_connection();
/// let unique_user_count = posts.select(count_distinct(user_id)).first(connection);
/// assert_eq!(Ok(2), unique_user_count);
/// # }
/// ```
pub fn count_distinct<T, E>(expr: E) -> CountDistinct<T, E::Expression>
where
    T: SqlType + SingleValue,
    E: AsExpression<T>,
{
    CountDistinct {
        expr: expr.as_expression(),
        _marker: PhantomData,
    }
}

#[derive(Debug, Clone, Copy, QueryId, DieselNumericOps)]
#[doc(hidden)]
pub struct CountDistinct<T, E> {
    expr: E,
    _marker: PhantomData<T>,
}

impl<T, E> Expression for CountDistinct<T, E>
where
    T: SqlType + SingleValue,
    E: Expression,
{
    type SqlType = BigInt;
}

impl<T, E, QS> SelectableExpression<QS> for CountDistinct<T, E>
where
    Self: AppearsInQuery<QS>,
    E: SelectableExpression<QS>,
{
}

impl<T, E, QS> AppearsInQuery<QS> for CountDistinct<T, E>
where
    Self: Expression,
    E: AppearsInQuery<QS>,
{
}

impl<T, E> QueryFragment for CountDistinct<T, E>
where
    T: SqlType + SingleValue,
    E: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_sql("COUNT(DISTINCT ");
        self.expr.walk_ast(out.reborrow())?;
        out.push_sql(")");
        Ok(())
    }
}
