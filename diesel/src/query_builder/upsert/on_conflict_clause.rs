use super::on_conflict_actions::*;
use super::on_conflict_target::*;
use crate::backend::sql_dialect;
use crate::backend::Backend;
use crate::backend::SqlDialect;
use crate::insertable::*;
use crate::query_builder::where_clause::{NoWhereClause, WhereClause};
use crate::query_builder::*;
use crate::result::QueryResult;

#[doc(hidden)]
#[derive(Debug, Clone, Copy)]
pub struct OnConflictValues<Values, Target, Action, WhereClause = NoWhereClause> {
    pub(crate) values: Values,
    pub(crate) target: Target,
    pub(crate) action: Action,
    /// Allow to apply filters on ON CONFLICT ... DO UPDATE ... WHERE ...
    pub(crate) where_clause: WhereClause,
}

impl<Values, Target, Action, WhereClause> QueryId
    for OnConflictValues<Values, Target, Action, WhereClause>
{
    type QueryId = ();

    const HAS_STATIC_QUERY_ID: bool = false;
}

impl<Values, T> OnConflictValues<Values, NoConflictTarget, DoNothing<T>, NoWhereClause> {
    pub(crate) fn do_nothing(values: Values) -> Self {
        Self::new(values, NoConflictTarget, DoNothing::new(), NoWhereClause)
    }
}

impl<Values, Target, Action, WhereClause> OnConflictValues<Values, Target, Action, WhereClause> {
    pub(crate) fn new(
        values: Values,
        target: Target,
        action: Action,
        where_clause: WhereClause,
    ) -> Self {
        OnConflictValues {
            values,
            target,
            action,
            where_clause,
        }
    }

    pub(crate) fn replace_where<Where, F>(
        self,
        f: F,
    ) -> OnConflictValues<Values, Target, Action, Where>
    where
        F: FnOnce(WhereClause) -> Where,
    {
        OnConflictValues::new(self.values, self.target, self.action, f(self.where_clause))
    }
}

impl<Values, Target, Action, WhereClause> CanInsertInSingleQuery
    for OnConflictValues<Values, Target, Action, WhereClause>
where
    Values: CanInsertInSingleQuery,
{
    fn rows_to_insert(&self) -> Option<usize> {
        self.values.rows_to_insert()
    }
}

type OnConflictClause = <DB as SqlDialect>::OnConflictClause;

impl<Values, Target, Action> QueryFragment
    for OnConflictValues<Values, Target, Action, NoWhereClause>
where
    Self: QueryFragment<OnConflictClause>,
{
    fn walk_ast<'b>(&'b self, pass: AstPass<'_, 'b>) -> QueryResult<()> {
        <Self as QueryFragment<OnConflictClause>>::walk_ast(self, pass)
    }
}

impl<Values, Target, Action, SD> QueryFragment<SD>
    for OnConflictValues<Values, Target, Action, NoWhereClause>
where
    DB: Backend<OnConflictClause = SD>,
    SD: sql_dialect::on_conflict_clause::PgLikeOnConflictClause,
    Values: QueryFragment,
    Target: QueryFragment,
    Action: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        self.values.walk_ast(out.reborrow())?;
        out.push_sql(" ON CONFLICT");
        self.target.walk_ast(out.reborrow())?;
        self.action.walk_ast(out.reborrow())?;
        Ok(())
    }
}

impl<Values, Target, Action, Expr> QueryFragment
    for OnConflictValues<Values, Target, Action, WhereClause<Expr>>
where
    Values: QueryFragment,
    Target: QueryFragment,
    Action: QueryFragment,
    WhereClause<Expr>: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        self.values.walk_ast(out.reborrow())?;
        out.push_sql(" ON CONFLICT");
        self.target.walk_ast(out.reborrow())?;
        self.action.walk_ast(out.reborrow())?;
        self.where_clause.walk_ast(out.reborrow())?;
        Ok(())
    }
}
