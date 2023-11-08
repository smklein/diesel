use super::ValuesClause;
use crate::backend::{sql_dialect, SqlDialect};
use crate::insertable::CanInsertInSingleQuery;
use crate::query_builder::{AstPass, DB, QueryFragment, QueryId};
use crate::QueryResult;
use std::marker::PhantomData;

/// This type represents a batch insert clause, which allows
/// to insert multiple rows at once.
///
/// Custom backends can specialize the [`QueryFragment`]
/// implementation via [`SqlDialect::BatchInsertSupport`]
/// or provide fully custom [`ExecuteDsl`](crate::query_dsl::methods::ExecuteDsl)
/// and [`LoadQuery`](crate::query_dsl::methods::LoadQuery) implementations
#[cfg_attr(
    feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes",
    cfg(feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes")
)]
#[derive(Debug)]
pub struct BatchInsert<V, Tab, QId, const STABLE_QUERY_ID: bool> {
    /// List of values that should be inserted
    pub values: V,
    _marker: PhantomData<(QId, Tab)>,
}

impl<V, Tab, QId, const STABLE_QUERY_ID: bool> BatchInsert<V, Tab, QId, STABLE_QUERY_ID> {
    pub(crate) fn new(values: V) -> Self {
        Self {
            values,
            _marker: PhantomData,
        }
    }
}

impl<V, QId: 'static, Tab: 'static, const STABLE_QUERY_ID: bool> QueryId
    for BatchInsert<V, Tab, QId, STABLE_QUERY_ID>
{
    type QueryId = QId;

    const HAS_STATIC_QUERY_ID: bool = STABLE_QUERY_ID;
}

impl<T, Table, QId, const HAS_STATIC_QUERY_ID: bool> CanInsertInSingleQuery
    for BatchInsert<T, Table, QId, HAS_STATIC_QUERY_ID>
where
    T: CanInsertInSingleQuery,
{
    fn rows_to_insert(&self) -> Option<usize> {
        self.values.rows_to_insert()
    }
}

impl<T, const N: usize> CanInsertInSingleQuery for [T; N] {
    fn rows_to_insert(&self) -> Option<usize> {
        Some(N)
    }
}

impl<T, const N: usize> CanInsertInSingleQuery for Box<[T; N]> {
    fn rows_to_insert(&self) -> Option<usize> {
        Some(N)
    }
}

impl<T> CanInsertInSingleQuery for [T] {
    fn rows_to_insert(&self) -> Option<usize> {
        Some(self.len())
    }
}

impl<T> CanInsertInSingleQuery for Vec<T> {
    fn rows_to_insert(&self) -> Option<usize> {
        Some(self.len())
    }
}

impl<Tab, V, QId, const HAS_STATIC_QUERY_ID: bool> QueryFragment
    for BatchInsert<V, Tab, QId, HAS_STATIC_QUERY_ID>
where
    Self: QueryFragment<<DB as SqlDialect>::BatchInsertSupport>,
{
    fn walk_ast<'b>(&'b self, pass: AstPass<'_, 'b, DB>) -> QueryResult<()> {
        <Self as QueryFragment<DB::BatchInsertSupport>>::walk_ast(self, pass)
    }
}

impl<Tab, V, QId, const HAS_STATIC_QUERY_ID: bool>
    QueryFragment<sql_dialect::batch_insert_support::PostgresLikeBatchInsertSupport>
    for BatchInsert<Vec<ValuesClause<V, Tab>>, Tab, QId, HAS_STATIC_QUERY_ID>
where
    ValuesClause<V, Tab>: QueryFragment,
    V: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b, DB>) -> QueryResult<()> {
        if !HAS_STATIC_QUERY_ID {
            out.unsafe_to_cache_prepared();
        }

        let mut values = self.values.iter();
        if let Some(value) = values.next() {
            value.walk_ast(out.reborrow())?;
        }
        for value in values {
            out.push_sql(", (");
            value.values.walk_ast(out.reborrow())?;
            out.push_sql(")");
        }
        Ok(())
    }
}
