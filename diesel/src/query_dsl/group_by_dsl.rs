use crate::dsl;
use crate::expression::Expression;
use crate::query_builder::FromClause;
use crate::query_builder::{AsQuery, SelectStatement};
use crate::query_source::Table;

/// The `group_by` method
///
/// This trait should not be relied on directly by most apps. Its behavior is
/// provided by [`QueryDsl`]. However, you may need a where clause on this trait
/// to call `group_by` from generic code.
///
/// [`QueryDsl`]: crate::QueryDsl
pub trait GroupByDsl<Expr: Expression> {
    /// The type returned by `.group_by`
    type Output;

    /// See the trait documentation.
    fn group_by(self, expr: Expr) -> dsl::GroupBy<Self, Expr>;
}

impl<'a, T, Expr> GroupByDsl<Expr> for T
where
    Expr: Expression,
    T: Table,
    T: AsQuery<Query = SelectStatement<'a, FromClause<T>>>,
    T::Query: GroupByDsl<Expr>,
{
    type Output = dsl::GroupBy<SelectStatement<'a, FromClause<T>>, Expr>;

    fn group_by(self, expr: Expr) -> dsl::GroupBy<Self, Expr> {
        self.as_query().group_by(expr)
    }
}
