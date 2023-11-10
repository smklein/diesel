//! Within this module, types commonly use the following abbreviations:
//!
//! F: From Clause
//! S: Select Clause
//! D: Distinct Clause
//! O: Order By Clause
//! L: Limit Clause
//! Of: Offset Clause
//! G: Group By Clause

pub(crate) mod boxed;
mod dsl_impls;
#[diesel_derives::__diesel_public_if(
    feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes"
)]
pub(crate) use self::boxed::BoxedSelectStatement;

use super::distinct_clause::NoDistinctClause;
use super::from_clause::AsQuerySource;
use super::from_clause::FromClause;
use super::group_by_clause::*;
use super::limit_clause::NoLimitClause;
use super::locking_clause::AllLockingClauses;
use super::offset_clause::NoOffsetClause;
use super::order_clause::NoOrderClause;
use super::select_clause::*;
use super::where_clause::*;
use super::NoFromClause;
use super::{AstPass, Query, QueryFragment};
use crate::expression::subselect::ValidSubselect;
use crate::expression::*;
use crate::query_builder::having_clause::HavingClause;
use crate::query_builder::limit_offset_clause::LimitOffsetClause;
use crate::query_builder::{QueryId, SelectQuery};
use crate::query_dsl::order_dsl::ValidOrderingForDistinct;
use crate::query_source::joins::{AppendSelection, Inner, Join};
use crate::query_source::*;
use crate::result::QueryResult;

/// This type represents a select query
///
/// Using this type directly is only meaningful for custom backends
/// that need to provide a custom [`QueryFragment`] implementation
#[diesel_derives::__diesel_public_if(
    feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes",
    public_fields(
        select,
        from,
        distinct,
        where_clause,
        order,
        limit_offset,
        group_by,
        having,
        locking
    )
)]
#[must_use = "Queries are only executed when calling `load`, `get_result` or similar."]
pub struct SelectStatement<
    'a,
    From,
    Select = DefaultSelectClause<From>,
    Distinct = NoDistinctClause,
    Order = NoOrderClause,
    LimitOffset = LimitOffsetClause<NoLimitClause, NoOffsetClause>,
    GroupBy = NoGroupByClause,
> {
    /// The select clause of the query
    pub(crate) select: Select,
    /// The from clause of the query
    pub(crate) from: From,
    /// The distinct clause of the query
    pub(crate) distinct: Distinct,
    /// The where clause of the query
    pub(crate) where_clause: BoxedWhereClause<'a>,
    /// The order clause of the query
    pub(crate) order: Order,
    /// The combined limit/offset clause of the query
    pub(crate) limit_offset: LimitOffset,
    /// The group by clause of the query
    pub(crate) group_by: GroupBy,
    /// The having clause of the query
    pub(crate) having: HavingClause,
    /// The locking clause of the query
    pub(crate) locking: AllLockingClauses,
}

impl<'a, F, S, D, O, LOf, G> QueryId for SelectStatement<'a, F, S, D, O, LOf, G> {
    type QueryId = ();

    const HAS_STATIC_QUERY_ID: bool = false;
}

impl<'a, F, S, D, O, LOf, G> SelectStatement<'a, F, S, D, O, LOf, G> {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn new(
        select: S,
        from: F,
        distinct: D,
        where_clause: BoxedWhereClause<'a>,
        order: O,
        limit_offset: LOf,
        group_by: G,
        having: HavingClause,
        locking: AllLockingClauses,
    ) -> Self {
        SelectStatement {
            select,
            from,
            distinct,
            where_clause,
            order,
            limit_offset,
            group_by,
            having,
            locking,
        }
    }
}

impl<'a, F: QuerySource> SelectStatement<'a, FromClause<F>> {
    // This is used by the `table!` macro
    #[doc(hidden)]
    pub fn simple(from: F) -> Self {
        let from = FromClause::new(from);
        SelectStatement::new(
            DefaultSelectClause::new(&from),
            from,
            NoDistinctClause,
            BoxedWhereClause::None,
            NoOrderClause,
            LimitOffsetClause {
                limit_clause: NoLimitClause,
                offset_clause: NoOffsetClause,
            },
            NoGroupByClause,
            HavingClause::None,
            AllLockingClauses::NoLocking,
        )
    }
}

impl<'a, F, S, D, O, LOf, G> Query for SelectStatement<'a, F, S, D, O, LOf, G>
where
    G: ValidGroupByClause,
    S: SelectClauseExpression<F>,
{
    type SqlType = S::SelectClauseSqlType;
}

impl<'a, F, S, D, O, LOf, G> SelectQuery for SelectStatement<'a, F, S, D, O, LOf, G>
where
    S: SelectClauseExpression<F>,
    O: ValidOrderingForDistinct<D>,
{
    type SqlType = S::SelectClauseSqlType;
}

impl<'a, F, S, D, O, LOf, G> QueryFragment for SelectStatement<'a, F, S, D, O, LOf, G>
where
    S: QueryFragment,
    F: QueryFragment,
    D: QueryFragment,
    O: QueryFragment,
    LOf: QueryFragment,
    G: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_sql("SELECT ");
        self.distinct.walk_ast(out.reborrow())?;
        self.select.walk_ast(out.reborrow())?;
        self.from.walk_ast(out.reborrow())?;
        self.where_clause.walk_ast(out.reborrow())?;
        self.group_by.walk_ast(out.reborrow())?;
        self.having.walk_ast(out.reborrow())?;
        self.order.walk_ast(out.reborrow())?;
        self.limit_offset.walk_ast(out.reborrow())?;
        self.locking.walk_ast(out.reborrow())?;
        Ok(())
    }
}

impl<'a, S, F, D, O, LOf, G, QS> ValidSubselect<QS>
    for SelectStatement<'a, FromClause<F>, S, D, O, LOf, G>
where
    Self: SelectQuery,
    F: QuerySource,
    QS: QuerySource,
    Join<F, QS, Inner>: QuerySource,
{
}

impl<'a, S, D, O, LOf, G> ValidSubselect<NoFromClause>
    for SelectStatement<'a, NoFromClause, S, D, O, LOf, G>
where
    Self: SelectQuery,
{
}

impl<'a, S, F, D, O, LOf, G> ValidSubselect<NoFromClause>
    for SelectStatement<'a, FromClause<F>, S, D, O, LOf, G>
where
    Self: SelectQuery,
    F: QuerySource,
{
}

impl<'a, S, D, O, LOf, G, QS> ValidSubselect<QS>
    for SelectStatement<'a, NoFromClause, S, D, O, LOf, G>
where
    Self: SelectQuery,
    QS: QuerySource,
{
}

/// Allow `SelectStatement<'a, From>` to act as if it were `From` as long as
/// no other query methods have been called on it
impl<'a, From, T> AppearsInFromClause<T> for SelectStatement<'a, From>
where
    From: AsQuerySource,
    From::QuerySource: AppearsInFromClause<T> + QuerySource,
{
    type Count = <From::QuerySource as AppearsInFromClause<T>>::Count;
}

impl<'a, From> QuerySource for SelectStatement<'a, From>
where
    From: AsQuerySource,
    <From::QuerySource as QuerySource>::DefaultSelection: SelectableExpression<Self>,
{
    type FromClause = <From::QuerySource as QuerySource>::FromClause;
    type DefaultSelection = <From::QuerySource as QuerySource>::DefaultSelection;

    fn from_clause(&self) -> <From::QuerySource as QuerySource>::FromClause {
        self.from.as_query_source().from_clause()
    }

    fn default_selection(&self) -> Self::DefaultSelection {
        self.from.as_query_source().default_selection()
    }
}

impl<'a, From, Selection> AppendSelection<Selection> for SelectStatement<'a, From>
where
    From: AsQuerySource,
    From::QuerySource: AppendSelection<Selection>,
{
    type Output = <From::QuerySource as AppendSelection<Selection>>::Output;

    fn append_selection(&self, selection: Selection) -> Self::Output {
        self.from.as_query_source().append_selection(selection)
    }
}
