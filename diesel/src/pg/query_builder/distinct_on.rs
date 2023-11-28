use crate::expression::SelectableExpression;
use crate::query_builder::{
    AstPass, FromClause, QueryFragment, QueryId, SelectQuery, SelectStatement,
};
use crate::query_dsl::methods::DistinctOnDsl;
use crate::result::QueryResult;
use crate::QuerySource;

/// Represents `DISTINCT ON (...)`
#[derive(Debug, Clone, Copy, QueryId)]
#[cfg(feature = "postgres_backend")]
pub struct DistinctOnClause<T>(pub(crate) T);

/// A decorator trait for `OrderClause`
/// It helps to have bounds on either Col, Asc<Col> and Desc<Col>.
pub trait OrderDecorator {
    /// A column on a database table.
    type Column;
}

impl<C> OrderDecorator for C
where
    C: crate::Column,
{
    type Column = C;
}

impl<C> OrderDecorator for crate::helper_types::Asc<C> {
    type Column = C;
}

impl<C> OrderDecorator for crate::helper_types::Desc<C> {
    type Column = C;
}

impl<T> QueryFragment for DistinctOnClause<T>
where
    T: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_sql("DISTINCT ON (");
        self.0.walk_ast(out.reborrow())?;
        out.push_sql(")");
        Ok(())
    }
}

impl<'a, ST, F, S, D, LOf, G, Selection> DistinctOnDsl<Selection>
    for SelectStatement<'a, FromClause<F>, S, D, LOf, G>
where
    F: QuerySource,
    Selection: SelectableExpression<F>,
    Self: SelectQuery<SqlType = ST>,
    SelectStatement<'a, FromClause<F>, S, DistinctOnClause<Selection>, LOf, G>:
        SelectQuery<SqlType = ST>,
{
    type Output = SelectStatement<'a, FromClause<F>, S, DistinctOnClause<Selection>, LOf, G>;

    fn distinct_on(self, selection: Selection) -> Self::Output {
        SelectStatement::new(
            self.select,
            self.from,
            DistinctOnClause(selection),
            self.where_clause,
            self.order,
            self.limit_offset,
            self.group_by,
            self.having,
            self.locking,
        )
    }
}
