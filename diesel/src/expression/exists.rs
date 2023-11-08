//! This module contains the query dsl node definition
//! for `EXISTS` expressions

use crate::backend::{sql_dialect, SqlDialect};
use crate::expression::subselect::Subselect;
use crate::expression::{AppearsInQuery, Expression, SelectableExpression};
use crate::helper_types::exists;
use crate::query_builder::*;
use crate::result::QueryResult;
use crate::sql_types::Bool;

/// Creates a SQL `EXISTS` expression.
///
/// The argument must be a complete SQL query. The query may reference columns
/// from the outer table.
///
/// # Example
///
/// ```rust
/// # include!("../doctest_setup.rs");
/// #
/// # fn main() {
/// #     use schema::users::dsl::*;
/// #     use diesel::select;
/// #     use diesel::dsl::exists;
/// #     let connection = &mut establish_connection();
/// let sean_exists = select(exists(users.filter(name.eq("Sean"))))
///     .get_result(connection);
/// let jim_exists = select(exists(users.filter(name.eq("Jim"))))
///     .get_result(connection);
/// assert_eq!(Ok(true), sean_exists);
/// assert_eq!(Ok(false), jim_exists);
/// # }
/// ```
pub fn exists<T>(query: T) -> exists<T> {
    Exists {
        subselect: Subselect::new(query),
    }
}

/// The query dsl node that represents a SQL `EXISTS (subselect)` expression.
///
/// Third party backend can customize the [`QueryFragment`]
/// implementation of this query dsl node via
/// [`SqlDialect::ExistsSyntax`]. A customized implementation
/// is expected to provide the same semantics as an ANSI SQL
/// `EXIST (subselect)` expression.
#[derive(Clone, Copy, QueryId, Debug)]
#[non_exhaustive]
pub struct Exists<T> {
    /// The inner subselect
    pub subselect: Subselect<T, Bool>,
}

impl<T> Expression for Exists<T>
where
    Subselect<T, Bool>: Expression,
{
    type SqlType = Bool;
}

impl<T> QueryFragment for Exists<T>
where
    Self: QueryFragment<<DB as SqlDialect>::ExistsSyntax>,
{
    fn walk_ast<'b>(&'b self, pass: AstPass<'_, 'b>) -> QueryResult<()> {
        <Self as QueryFragment<<DB as SqlDialect>::ExistsSyntax>>::walk_ast(self, pass)
    }
}

impl<T> QueryFragment<sql_dialect::exists_syntax::AnsiSqlExistsSyntax> for Exists<T>
where
    T: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_sql("EXISTS (");
        self.subselect.walk_ast(out.reborrow())?;
        out.push_sql(")");
        Ok(())
    }
}

impl<T, QS> SelectableExpression<QS> for Exists<T>
where
    Self: AppearsInQuery<QS>,
    Subselect<T, Bool>: SelectableExpression<QS>,
{
}

impl<T, QS> AppearsInQuery<QS> for Exists<T>
where
    Self: Expression,
    Subselect<T, Bool>: AppearsInQuery<QS>,
{
}
