use crate::dsl;
use crate::expression::TypedExpressionType;
use crate::query_builder::AsQuery;
use crate::query_builder::FromClause;
use crate::query_builder::SelectStatement;
use crate::query_source::Table;
use crate::Expression;

/// The `into_boxed` method
///
/// This trait should not be relied on directly by most apps. Its behavior is
/// provided by [`QueryDsl`]. However, you may need a where clause on this trait
/// to call `into_boxed` from generic code.
///
/// [`QueryDsl`]: crate::QueryDsl
pub trait BoxedDsl<'a> {
    /// The return type of `internal_into_boxed`
    type Output;

    /// See the trait documentation.
    fn internal_into_boxed(self) -> dsl::IntoBoxed<'a, Self>;
}

impl<'a, T> BoxedDsl<'a> for T
where
    T: Table + AsQuery<Query = SelectStatement<'a, FromClause<T>>>,
    SelectStatement<'a, FromClause<T>>: BoxedDsl<'a>,
    T::DefaultSelection: Expression<SqlType = T::SqlType>,
    T::SqlType: TypedExpressionType,
{
    type Output = dsl::IntoBoxed<'a, SelectStatement<'a, FromClause<T>>>;

    fn internal_into_boxed(self) -> Self::Output {
        self.as_query().internal_into_boxed()
    }
}
