use super::from_clause::AsQuerySource;
use super::*;
use crate::expression::grouped::Grouped;
use crate::expression::operators::{And, Or};
use crate::expression::*;
use crate::result::QueryResult;
use crate::sql_types::BoolOrNullableBool;

/// Add `Predicate` to the current `WHERE` clause, joining with `AND` if
/// applicable.
pub trait WhereAnd<Predicate> {
    /// What is the type of the resulting `WHERE` clause?
    type Output;

    /// See the trait-level docs.
    fn and(self, predicate: Predicate) -> Self::Output;
}

/// Add `Predicate` to the current `WHERE` clause, joining with `OR` if
/// applicable.
pub trait WhereOr<Predicate> {
    /// What is the type of the resulting `WHERE` clause?
    type Output;

    /// See the trait-level docs.
    fn or(self, predicate: Predicate) -> Self::Output;
}

/// Represents that a query has no `WHERE` clause.
#[derive(Debug, Clone, Copy, QueryId)]
pub struct NoWhereClause;

impl QueryFragment for NoWhereClause {
    fn walk_ast<'b>(&'b self, _: AstPass<'_, 'b>) -> QueryResult<()> {
        Ok(())
    }
}

impl<Predicate> WhereAnd<Predicate> for NoWhereClause
where
    Predicate: Expression,
    Predicate::SqlType: BoolOrNullableBool,
{
    type Output = WhereClause<Predicate>;

    fn and(self, predicate: Predicate) -> Self::Output {
        WhereClause(predicate)
    }
}

impl<Predicate> WhereOr<Predicate> for NoWhereClause
where
    Predicate: Expression,
    Predicate::SqlType: BoolOrNullableBool,
{
    type Output = WhereClause<Predicate>;

    fn or(self, predicate: Predicate) -> Self::Output {
        WhereClause(predicate)
    }
}

impl<'a> From<NoWhereClause> for BoxedWhereClause<'a> {
    fn from(_: NoWhereClause) -> Self {
        BoxedWhereClause::None
    }
}

/// The `WHERE` clause of a query.
#[derive(Debug, Clone, Copy, QueryId)]
pub struct WhereClause<Expr>(Expr);

impl<Expr> QueryFragment for WhereClause<Expr>
where
    Expr: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_sql(" WHERE ");
        self.0.walk_ast(out.reborrow())?;
        Ok(())
    }
}

impl<Expr, Predicate> WhereAnd<Predicate> for WhereClause<Expr>
where
    Expr: Expression,
    Expr::SqlType: BoolOrNullableBool,
    Predicate: Expression,
    Predicate::SqlType: BoolOrNullableBool,
{
    type Output = WhereClause<Grouped<And<Expr, Predicate>>>;

    fn and(self, predicate: Predicate) -> Self::Output {
        WhereClause(Grouped(And::new(self.0, predicate)))
    }
}

impl<Expr, Predicate> WhereOr<Predicate> for WhereClause<Expr>
where
    Expr: Expression,
    Expr::SqlType: BoolOrNullableBool,
    Predicate: Expression,
    Predicate::SqlType: BoolOrNullableBool,
{
    type Output = WhereClause<Grouped<Or<Expr, Predicate>>>;

    fn or(self, predicate: Predicate) -> Self::Output {
        WhereClause(Grouped(Or::new(self.0, predicate)))
    }
}

impl<'a, Predicate> From<WhereClause<Predicate>> for BoxedWhereClause<'a>
where
    Predicate: QueryFragment + Send + 'a,
{
    fn from(where_clause: WhereClause<Predicate>) -> Self {
        BoxedWhereClause::Where(Box::new(where_clause.0))
    }
}

/// Marker trait indicating that a `WHERE` clause is valid for a given query
/// source.
trait ValidWhereClause<QS> {}

impl<QS> ValidWhereClause<QS> for NoWhereClause {}

impl<QS, Expr> ValidWhereClause<QS> for WhereClause<Expr>
where
    Expr: AppearsInQuery<QS::QuerySource>,
    QS: AsQuerySource,
{
}

impl<Expr> ValidWhereClause<NoFromClause> for WhereClause<Expr> where
    Expr: AppearsInQuery<NoFromClause>
{
}

#[allow(missing_debug_implementations)] // We can't...
pub enum BoxedWhereClause<'a> {
    Where(Box<dyn QueryFragment + Send + 'a>),
    None,
}

impl<'a> QueryFragment for BoxedWhereClause<'a> {
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        match *self {
            BoxedWhereClause::Where(ref where_clause) => {
                out.push_sql(" WHERE ");
                where_clause.walk_ast(out)
            }
            BoxedWhereClause::None => Ok(()),
        }
    }
}

impl<'a> QueryId for BoxedWhereClause<'a> {
    type QueryId = ();

    const HAS_STATIC_QUERY_ID: bool = false;
}

impl<'a, Predicate> WhereAnd<Predicate> for BoxedWhereClause<'a>
where
    Predicate: QueryFragment + Send + 'a,
    Grouped<And<Box<dyn QueryFragment + Send + 'a>, Predicate>>: QueryFragment,
{
    type Output = Self;

    fn and(self, predicate: Predicate) -> Self::Output {
        use self::BoxedWhereClause::Where;

        match self {
            Where(where_clause) => Where(Box::new(Grouped(And::new(where_clause, predicate)))),
            BoxedWhereClause::None => Where(Box::new(predicate)),
        }
    }
}

impl<'a, Predicate> WhereOr<Predicate> for BoxedWhereClause<'a>
where
    Predicate: QueryFragment + Send + 'a,
    Grouped<Or<Box<dyn QueryFragment + Send + 'a>, Predicate>>: QueryFragment,
{
    type Output = Self;

    fn or(self, predicate: Predicate) -> Self::Output {
        use self::BoxedWhereClause::Where;

        match self {
            Where(where_clause) => Where(Box::new(Grouped(Or::new(where_clause, predicate)))),
            BoxedWhereClause::None => Where(Box::new(predicate)),
        }
    }
}
