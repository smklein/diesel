use crate::dsl;
#[cfg(feature = "postgres_backend")]
use crate::expression::SelectableExpression;
use crate::expression::TypedExpressionType;
use crate::query_builder::FromClause;
use crate::query_builder::{AsQuery, SelectStatement};
use crate::query_source::Table;
use crate::Expression;

/// The `distinct` method
///
/// This trait should not be relied on directly by most apps. Its behavior is
/// provided by [`QueryDsl`]. However, you may need a where clause on this trait
/// to call `distinct` from generic code.
///
/// [`QueryDsl`]: crate::QueryDsl
pub trait DistinctDsl {
    /// The type returned by `.distinct`
    type Output;

    /// See the trait documentation.
    fn distinct(self) -> dsl::Distinct<Self>;
}

impl<'a, T> DistinctDsl for T
where
    T: Table + AsQuery<Query = SelectStatement<'a, FromClause<T>>>,
    T::DefaultSelection: Expression<SqlType = T::SqlType>,
    T::SqlType: TypedExpressionType,
{
    type Output = dsl::Distinct<SelectStatement<'a, FromClause<T>>>;

    fn distinct(self) -> dsl::Distinct<SelectStatement<'a, FromClause<T>>> {
        self.as_query().distinct()
    }
}

/// The `distinct_on` method
///
/// This trait should not be relied on directly by most apps. Its behavior is
/// provided by [`QueryDsl`]. However, you may need a where clause on this trait
/// to call `distinct_on` from generic code.
///
/// [`QueryDsl`]: crate::QueryDsl
#[cfg(feature = "postgres_backend")]
pub trait DistinctOnDsl<Selection> {
    /// The type returned by `.distinct_on`
    type Output;

    /// See the trait documentation
    fn distinct_on(self, selection: Selection) -> dsl::DistinctOn<Self, Selection>;
}

#[cfg(feature = "postgres_backend")]
impl<'a, T, Selection> DistinctOnDsl<Selection> for T
where
    Selection: SelectableExpression<T>,
    T: Table + AsQuery<Query = SelectStatement<'a, FromClause<T>>>,
    SelectStatement<'a, FromClause<T>>: DistinctOnDsl<Selection>,
    T::DefaultSelection: Expression<SqlType = T::SqlType>,
    T::SqlType: TypedExpressionType,
{
    type Output = dsl::DistinctOn<SelectStatement<'a, FromClause<T>>, Selection>;

    fn distinct_on(self, selection: Selection) -> dsl::DistinctOn<Self, Selection> {
        self.as_query().distinct_on(selection)
    }
}
