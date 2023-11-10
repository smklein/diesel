use crate::expression::TypedExpressionType;
use crate::query_builder::locking_clause::AllLockingClauses;
use crate::query_builder::AsQuery;
use crate::query_builder::FromClause;
use crate::query_builder::SelectStatement;
use crate::query_source::Table;
use crate::Expression;

/// Methods related to locking select statements
///
/// This trait should not be relied on directly by most apps. Its behavior is
/// provided by [`QueryDsl`]. However, you may need a where clause on this trait
/// to call `for_update` from generic code.
///
/// [`QueryDsl`]: crate::QueryDsl
pub trait LockingDsl {
    /// The type returned by `set_lock`. See [`dsl::ForUpdate`] and friends for
    /// convenient access to this type.
    ///
    /// [`dsl::ForUpdate`]: crate::dsl::ForUpdate
    type Output;

    /// See the trait level documentation
    fn with_lock(self, lock: AllLockingClauses) -> Self::Output;
}

impl<'a, T> LockingDsl for T
where
    T: Table + AsQuery<Query = SelectStatement<'a, FromClause<T>>>,
    T::DefaultSelection: Expression<SqlType = T::SqlType>,
    T::SqlType: TypedExpressionType,
{
    type Output = <SelectStatement<'a, FromClause<T>> as LockingDsl>::Output;

    fn with_lock(self, lock: AllLockingClauses) -> Self::Output {
        self.as_query().with_lock(lock)
    }
}
