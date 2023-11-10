use super::{AstPass, DB, QueryFragment};
use crate::backend::{sql_dialect, SqlDialect};
use crate::query_builder::QueryId;
use crate::result::QueryResult;

#[derive(Debug, Clone, Copy, QueryId)]
pub struct NoReturningClause;

impl QueryFragment for NoReturningClause {
    fn walk_ast<'b>(&'b self, _: AstPass<'_, 'b>) -> QueryResult<()> {
        Ok(())
    }
}

/// This type represents a SQL `Returning` clause
#[cfg_attr(
    feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes",
    cfg(feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes")
)]
#[derive(Debug, Clone, Copy, QueryId)]
pub struct ReturningClause<Expr>(pub Expr);

type ReturningClauseSyntax = <DB as SqlDialect>::ReturningClause;

impl<Expr> QueryFragment for ReturningClause<Expr>
where
    ReturningClauseSyntax: sql_dialect::returning_clause::SupportsReturningClause,
    Expr: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_sql(" RETURNING ");
        self.0.walk_ast(out.reborrow())?;
        Ok(())
    }
}
