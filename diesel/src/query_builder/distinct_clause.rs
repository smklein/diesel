use crate::query_builder::*;
use crate::query_dsl::order_dsl::ValidOrderingForDistinct;
use crate::result::QueryResult;

#[derive(Debug, Clone, Copy, QueryId)]
pub struct NoDistinctClause;
#[derive(Debug, Clone, Copy, QueryId)]
pub struct DistinctClause;

impl QueryFragment for NoDistinctClause {
    fn walk_ast<'b>(&'b self, _: AstPass<'_, 'b>) -> QueryResult<()> {
        Ok(())
    }
}

impl QueryFragment for DistinctClause {
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_sql("DISTINCT ");
        Ok(())
    }
}

impl<O> ValidOrderingForDistinct<NoDistinctClause> for O {}
impl<O> ValidOrderingForDistinct<DistinctClause> for O {}

// This is rexported from another location
#[allow(unreachable_pub)]
#[cfg(feature = "postgres_backend")]
pub use crate::pg::DistinctOnClause;
