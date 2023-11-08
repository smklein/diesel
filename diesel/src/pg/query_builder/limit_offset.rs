use crate::query_builder::limit_offset_clause::{BoxedLimitOffsetClause, LimitOffsetClause};
use crate::query_builder::{AstPass, IntoBoxedClause, QueryFragment};
use crate::result::QueryResult;

impl<'a, L, O> IntoBoxedClause<'a> for LimitOffsetClause<L, O>
where
    L: QueryFragment + Send + 'a,
    O: QueryFragment + Send + 'a,
{
    type BoxedClause = BoxedLimitOffsetClause<'a>;

    fn into_boxed(self) -> Self::BoxedClause {
        BoxedLimitOffsetClause {
            limit: Some(Box::new(self.limit_clause)),
            offset: Some(Box::new(self.offset_clause)),
        }
    }
}

impl<'a> QueryFragment for BoxedLimitOffsetClause<'a> {
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        if let Some(ref limit) = self.limit {
            limit.walk_ast(out.reborrow())?;
        }
        if let Some(ref offset) = self.offset {
            offset.walk_ast(out.reborrow())?;
        }
        Ok(())
    }
}

impl<L, O> QueryFragment for LimitOffsetClause<L, O>
where
    L: QueryFragment,
    O: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        self.limit_clause.walk_ast(out.reborrow())?;
        self.offset_clause.walk_ast(out.reborrow())?;
        Ok(())
    }
}
