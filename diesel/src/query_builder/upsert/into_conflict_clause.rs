use crate::query_builder::insert_statement::{BatchInsert, InsertFromSelect};
use crate::query_builder::{BoxedSelectStatement, Query, SelectStatement, ValuesClause};

pub trait IntoConflictValueClause {
    type ValueClause;

    fn into_value_clause(self) -> Self::ValueClause;
}

#[derive(Debug, Clone, Copy)]
pub struct OnConflictSelectWrapper<S>(pub(crate) S);

impl<Q> Query for OnConflictSelectWrapper<Q>
where
    Q: Query,
{
    type SqlType = Q::SqlType;
}

impl<Inner, Tab> IntoConflictValueClause for ValuesClause<Inner, Tab> {
    type ValueClause = Self;

    fn into_value_clause(self) -> Self::ValueClause {
        self
    }
}

impl<V, Tab, QId, const STATIC_QUERY_ID: bool> IntoConflictValueClause
    for BatchInsert<V, Tab, QId, STATIC_QUERY_ID>
{
    type ValueClause = Self;

    fn into_value_clause(self) -> Self::ValueClause {
        self
    }
}

impl<'a, F, S, D, O, LOf, G, Columns> IntoConflictValueClause
    for InsertFromSelect<SelectStatement<'a, F, S, D, O, LOf, G>, Columns>
{
    type ValueClause = InsertFromSelect<
        OnConflictSelectWrapper<SelectStatement<'a, F, S, D, O, LOf, G>>,
        Columns,
    >;

    fn into_value_clause(self) -> Self::ValueClause {
        let InsertFromSelect { columns, query } = self;
        InsertFromSelect {
            query: OnConflictSelectWrapper(query),
            columns,
        }
    }
}

impl<'a, ST, QS, GB, Columns> IntoConflictValueClause
    for InsertFromSelect<BoxedSelectStatement<'a, ST, QS, GB>, Columns>
{
    type ValueClause = InsertFromSelect<
        OnConflictSelectWrapper<BoxedSelectStatement<'a, ST, QS, GB>>,
        Columns,
    >;

    fn into_value_clause(self) -> Self::ValueClause {
        let InsertFromSelect { columns, query } = self;
        InsertFromSelect {
            query: OnConflictSelectWrapper(query),
            columns,
        }
    }
}
