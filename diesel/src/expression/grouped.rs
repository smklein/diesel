use crate::expression::Expression;
use crate::query_builder::*;
use crate::result::QueryResult;
use crate::sql_types::DieselNumericOps;

#[derive(Debug, Copy, Clone, QueryId, Default, DieselNumericOps)]
pub struct Grouped<T>(pub T);

impl<T: Expression> Expression for Grouped<T> {
    type SqlType = T::SqlType;
}

impl<T> QueryFragment for Grouped<T>
where
    T: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_sql("(");
        self.0.walk_ast(out.reborrow())?;
        out.push_sql(")");
        Ok(())
    }
}

impl_selectable_expression!(Grouped<T>);
