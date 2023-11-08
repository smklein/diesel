use std::marker::PhantomData;

use crate::expression::array_comparison::MaybeEmpty;
use crate::expression::*;
use crate::query_builder::*;
use crate::result::QueryResult;
use crate::sql_types::SqlType;

#[derive(Debug, Copy, Clone, QueryId)]
pub struct Subselect<T, ST> {
    values: T,
    _sql_type: PhantomData<ST>,
}

impl<T, ST> Subselect<T, ST> {
    pub(crate) fn new(values: T) -> Self {
        Self {
            values,
            _sql_type: PhantomData,
        }
    }
}

impl<T: SelectQuery, ST> Expression for Subselect<T, ST>
where
    ST: SqlType + TypedExpressionType,
{
    type SqlType = ST;
}

impl<T, ST> MaybeEmpty for Subselect<T, ST> {
    fn is_empty(&self) -> bool {
        false
    }
}

impl<T, ST, QS> SelectableExpression<QS> for Subselect<T, ST>
where
    Subselect<T, ST>: AppearsInQuery<QS>,
    T: ValidSubselect<QS>,
{
}

impl<T, ST, QS> AppearsInQuery<QS> for Subselect<T, ST>
where
    Subselect<T, ST>: Expression,
    T: ValidSubselect<QS>,
{
}

impl<T, ST> QueryFragment for Subselect<T, ST>
where
    T: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        self.values.walk_ast(out.reborrow())?;
        Ok(())
    }
}

pub trait ValidSubselect<QS> {}
