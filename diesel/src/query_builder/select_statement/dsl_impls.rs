use super::BoxedSelectStatement;
use crate::associations::HasTable;
use crate::dsl::AsExprOf;
use crate::expression::nullable::Nullable;
use crate::expression::*;
use crate::insertable::Insertable;
use crate::query_builder::combination_clause::*;
use crate::query_builder::distinct_clause::*;
use crate::query_builder::from_clause::AsQuerySource;
use crate::query_builder::from_clause::FromClause;
use crate::query_builder::group_by_clause::*;
use crate::query_builder::insert_statement::InsertFromSelect;
use crate::query_builder::limit_clause::*;
use crate::query_builder::limit_offset_clause::{BoxedLimitOffsetClause, LimitOffsetClause};
use crate::query_builder::locking_clause::*;
use crate::query_builder::offset_clause::*;
use crate::query_builder::order_clause::*;
use crate::query_builder::select_clause::*;
use crate::query_builder::update_statement::target::*;
use crate::query_builder::where_clause::*;
use crate::query_builder::NoFromClause;
use crate::query_builder::{
    AsQuery, IntoBoxedClause, Query, QueryFragment, SelectQuery, SelectStatement,
};
use crate::query_dsl::boxed_dsl::BoxedDsl;
use crate::query_dsl::methods::*;
use crate::query_dsl::order_dsl::ValidOrderingForDistinct;
use crate::query_dsl::*;
use crate::query_source::joins::{Join, JoinOn, JoinTo};
use crate::query_source::QuerySource;
use crate::sql_types::{BigInt, BoolOrNullableBool};

impl<F, D, W, O, LOf, G, Rhs, Kind, On> InternalJoinDsl<Rhs, Kind, On>
    for SelectStatement<FromClause<F>, DefaultSelectClause<FromClause<F>>, D, W, O, LOf, G>
where
    F: QuerySource,
    Rhs: QuerySource,
    JoinOn<Join<F, Rhs, Kind>, On>: QuerySource,
    SelectStatement<
        FromClause<JoinOn<Join<F, Rhs, Kind>, On>>,
        DefaultSelectClause<FromClause<JoinOn<Join<F, Rhs, Kind>, On>>>,
        D,
        W,
        O,
        LOf,
        G,
    >: AsQuery,
{
    type Output = SelectStatement<
        FromClause<JoinOn<Join<F, Rhs, Kind>, On>>,
        DefaultSelectClause<FromClause<JoinOn<Join<F, Rhs, Kind>, On>>>,
        D,
        W,
        O,
        LOf,
        G,
    >;

    fn join(self, rhs: Rhs, kind: Kind, on: On) -> Self::Output {
        let from = FromClause::new(Join::new(self.from.source, rhs, kind).on(on));
        SelectStatement::new(
            DefaultSelectClause::new(&from),
            from,
            self.distinct,
            self.where_clause,
            self.order,
            self.limit_offset,
            self.group_by,
            self.having,
            self.locking,
        )
    }
}

impl<F, S, D, W, O, LOf, G, Rhs, Kind, On> InternalJoinDsl<Rhs, Kind, On>
    for SelectStatement<FromClause<F>, SelectClause<S>, D, W, O, LOf, G>
where
    F: QuerySource,
    Rhs: QuerySource,
    JoinOn<Join<F, Rhs, Kind>, On>: QuerySource,
    SelectStatement<
        FromClause<JoinOn<Join<F, Rhs, Kind>, On>>,
        SelectClause<S>,
        D,
        W,
        O,
        LOf,
        G,
    >: AsQuery,
{
    type Output = SelectStatement<
        FromClause<JoinOn<Join<F, Rhs, Kind>, On>>,
        SelectClause<S>,
        D,
        W,
        O,
        LOf,
        G,
    >;

    fn join(self, rhs: Rhs, kind: Kind, on: On) -> Self::Output {
        SelectStatement::new(
            self.select,
            FromClause::new(Join::new(self.from.source, rhs, kind).on(on)),
            self.distinct,
            self.where_clause,
            self.order,
            self.limit_offset,
            self.group_by,
            self.having,
            self.locking,
        )
    }
}

impl<F, S, D, W, O, LOf, G, Selection> SelectDsl<Selection>
    for SelectStatement<FromClause<F>, S, D, W, O, LOf, G>
where
    G: ValidGroupByClause,
    F: QuerySource,
    Selection: SelectableExpression<F>,
    SelectStatement<FromClause<F>, SelectClause<Selection>, D, W, O, LOf, G>: SelectQuery,
{
    type Output = SelectStatement<FromClause<F>, SelectClause<Selection>, D, W, O, LOf, G>;

    fn select(self, selection: Selection) -> Self::Output {
        SelectStatement::new(
            SelectClause(selection),
            self.from,
            self.distinct,
            self.where_clause,
            self.order,
            self.limit_offset,
            self.group_by,
            self.having,
            self.locking,
        )
    }
}

impl<S, D, W, O, LOf, G, Selection> SelectDsl<Selection>
    for SelectStatement<NoFromClause, S, D, W, O, LOf, G>
where
    G: ValidGroupByClause,
    Selection: SelectableExpression<NoFromClause>,
    SelectStatement<NoFromClause, SelectClause<Selection>, D, W, O, LOf, G>: SelectQuery,
{
    type Output = SelectStatement<NoFromClause, SelectClause<Selection>, D, W, O, LOf, G>;

    fn select(self, selection: Selection) -> Self::Output {
        SelectStatement::new(
            SelectClause(selection),
            self.from,
            self.distinct,
            self.where_clause,
            self.order,
            self.limit_offset,
            self.group_by,
            self.having,
            self.locking,
        )
    }
}

impl<ST, F, S, D, W, O, LOf, G> DistinctDsl for SelectStatement<F, S, D, W, O, LOf, G>
where
    Self: SelectQuery<SqlType = ST>,
    SelectStatement<F, S, DistinctClause, W, O, LOf, G>: SelectQuery<SqlType = ST>,
{
    type Output = SelectStatement<F, S, DistinctClause, W, O, LOf, G>;

    fn distinct(self) -> Self::Output {
        SelectStatement::new(
            self.select,
            self.from,
            DistinctClause,
            self.where_clause,
            self.order,
            self.limit_offset,
            self.group_by,
            self.having,
            self.locking,
        )
    }
}

impl<F, S, D, W, O, LOf, G, Predicate> FilterDsl<Predicate>
    for SelectStatement<F, S, D, W, O, LOf, G>
where
    Predicate: Expression,
    Predicate::SqlType: BoolOrNullableBool,
    W: WhereAnd<Predicate>,
{
    type Output = SelectStatement<F, S, D, W::Output, O, LOf, G>;

    fn filter(self, predicate: Predicate) -> Self::Output {
        SelectStatement::new(
            self.select,
            self.from,
            self.distinct,
            self.where_clause.and(predicate),
            self.order,
            self.limit_offset,
            self.group_by,
            self.having,
            self.locking,
        )
    }
}

impl<F, S, D, W, O, LOf, G, Predicate> OrFilterDsl<Predicate>
    for SelectStatement<F, S, D, W, O, LOf, G>
where
    Predicate: Expression,
    Predicate::SqlType: BoolOrNullableBool,
    W: WhereOr<Predicate>,
{
    type Output = SelectStatement<F, S, D, W::Output, O, LOf, G>;

    fn or_filter(self, predicate: Predicate) -> Self::Output {
        SelectStatement::new(
            self.select,
            self.from,
            self.distinct,
            self.where_clause.or(predicate),
            self.order,
            self.limit_offset,
            self.group_by,
            self.having,
            self.locking,
        )
    }
}

use crate::dsl::Filter;
use crate::expression_methods::EqAll;
use crate::query_builder::having_clause::HavingClause;
use crate::query_source::Table;

impl<F, S, D, W, O, LOf, G, PK> FindDsl<PK>
    for SelectStatement<FromClause<F>, S, D, W, O, LOf, G>
where
    F: Table,
    F::PrimaryKey: EqAll<PK>,
    Self: FilterDsl<<F::PrimaryKey as EqAll<PK>>::Output>,
{
    type Output = Filter<Self, <F::PrimaryKey as EqAll<PK>>::Output>;

    fn find(self, id: PK) -> Self::Output {
        let primary_key = self.from.source.primary_key();
        FilterDsl::filter(self, primary_key.eq_all(id))
    }
}

// no impls for `NoFromClause` here because order is not really supported there yet
impl<ST, F, S, D, W, O, LOf, G, Expr> OrderDsl<Expr>
    for SelectStatement<FromClause<F>, S, D, W, O, LOf, G>
where
    F: QuerySource,
    Expr: AppearsInQuery<F>,
    Self: SelectQuery<SqlType = ST>,
    SelectStatement<FromClause<F>, S, D, W, OrderClause<Expr>, LOf, G>:
        SelectQuery<SqlType = ST>,
    OrderClause<Expr>: ValidOrderingForDistinct<D>,
{
    type Output = SelectStatement<FromClause<F>, S, D, W, OrderClause<Expr>, LOf, G>;

    fn order(self, expr: Expr) -> Self::Output {
        let order = OrderClause(expr);
        SelectStatement::new(
            self.select,
            self.from,
            self.distinct,
            self.where_clause,
            order,
            self.limit_offset,
            self.group_by,
            self.having,
            self.locking,
        )
    }
}

impl<F, S, D, W, O, LOf, G, Expr> ThenOrderDsl<Expr>
    for SelectStatement<FromClause<F>, S, D, W, OrderClause<O>, LOf, G>
where
    F: QuerySource,
    Expr: AppearsInQuery<F>,
{
    type Output = SelectStatement<FromClause<F>, S, D, W, OrderClause<(O, Expr)>, LOf, G>;

    fn then_order_by(self, expr: Expr) -> Self::Output {
        SelectStatement::new(
            self.select,
            self.from,
            self.distinct,
            self.where_clause,
            OrderClause((self.order.0, expr)),
            self.limit_offset,
            self.group_by,
            self.having,
            self.locking,
        )
    }
}

impl<F, S, D, W, LOf, G, Expr> ThenOrderDsl<Expr>
    for SelectStatement<F, S, D, W, NoOrderClause, LOf, G>
where
    Expr: Expression,
    Self: OrderDsl<Expr>,
{
    type Output = crate::dsl::Order<Self, Expr>;

    fn then_order_by(self, expr: Expr) -> Self::Output {
        self.order_by(expr)
    }
}

#[doc(hidden)]
type Limit = AsExprOf<i64, BigInt>;

impl<ST, F, S, D, W, O, L, Of, G> LimitDsl
    for SelectStatement<F, S, D, W, O, LimitOffsetClause<L, Of>, G>
where
    Self: SelectQuery<SqlType = ST>,
    SelectStatement<F, S, D, W, O, LimitOffsetClause<LimitClause<Limit>, Of>, G>:
        SelectQuery<SqlType = ST>,
{
    type Output =
        SelectStatement<F, S, D, W, O, LimitOffsetClause<LimitClause<Limit>, Of>, G>;

    fn limit(self, limit: i64) -> Self::Output {
        let limit_clause = LimitClause(limit.into_sql::<BigInt>());
        SelectStatement::new(
            self.select,
            self.from,
            self.distinct,
            self.where_clause,
            self.order,
            LimitOffsetClause {
                limit_clause,
                offset_clause: self.limit_offset.offset_clause,
            },
            self.group_by,
            self.having,
            self.locking,
        )
    }
}

#[doc(hidden)]
type Offset = Limit;

impl<ST, F, S, D, W, O, L, Of, G> OffsetDsl
    for SelectStatement<F, S, D, W, O, LimitOffsetClause<L, Of>, G>
where
    Self: SelectQuery<SqlType = ST>,
    SelectStatement<F, S, D, W, O, LimitOffsetClause<L, OffsetClause<Offset>>, G>:
        SelectQuery<SqlType = ST>,
{
    type Output =
        SelectStatement<F, S, D, W, O, LimitOffsetClause<L, OffsetClause<Offset>>, G>;

    fn offset(self, offset: i64) -> Self::Output {
        let offset_clause = OffsetClause(offset.into_sql::<BigInt>());
        SelectStatement::new(
            self.select,
            self.from,
            self.distinct,
            self.where_clause,
            self.order,
            LimitOffsetClause {
                limit_clause: self.limit_offset.limit_clause,
                offset_clause,
            },
            self.group_by,
            self.having,
            self.locking,
        )
    }
}

impl<F, S, D, W, O, LOf, G, Expr> GroupByDsl<Expr> for SelectStatement<F, S, D, W, O, LOf, G>
where
    SelectStatement<F, S, D, W, O, LOf, GroupByClause<Expr>>: SelectQuery,
    Expr: Expression + AppearsInQuery<F>,
{
    type Output = SelectStatement<F, S, D, W, O, LOf, GroupByClause<Expr>>;

    fn group_by(self, expr: Expr) -> Self::Output {
        let group_by = GroupByClause(expr);
        SelectStatement::new(
            self.select,
            self.from,
            self.distinct,
            self.where_clause,
            self.order,
            self.limit_offset,
            group_by,
            self.having,
            self.locking,
        )
    }
}

impl<F, S, W, O, LOf> LockingDsl
    for SelectStatement<F, S, NoDistinctClause, W, O, LOf>
{
    type Output = SelectStatement<
        F,
        S,
        NoDistinctClause,
        W,
        O,
        LOf,
        NoGroupByClause,
    >;

    fn with_lock(self, lock: AllLockingClauses) -> Self::Output {
        SelectStatement::new(
            self.select,
            self.from,
            self.distinct,
            self.where_clause,
            self.order,
            self.limit_offset,
            self.group_by,
            self.having,
            lock,
        )
    }
}

impl<'a, F, S, D, W, O, LOf, G> BoxedDsl<'a>
    for SelectStatement<FromClause<F>, S, D, W, O, LOf, G>
where
    Self: AsQuery,
    F: QuerySource,
    S: SelectClauseExpression<FromClause<F>> + QueryFragment + Send + 'a,
    D: QueryFragment + Send + 'a,
    W: Into<BoxedWhereClause<'a>>,
    O: Into<Option<Box<dyn QueryFragment + Send + 'a>>>,
    LOf: IntoBoxedClause<'a, BoxedClause = BoxedLimitOffsetClause<'a>>,
    G: ValidGroupByClause + QueryFragment + Send + 'a,
{
    type Output =
        BoxedSelectStatement<'a, S::SelectClauseSqlType, FromClause<F>, G::Expressions>;

    fn internal_into_boxed(self) -> Self::Output {
        BoxedSelectStatement::new(
            self.select,
            self.from,
            Box::new(self.distinct),
            self.where_clause.into(),
            self.order.into(),
            self.limit_offset.into_boxed(),
            self.group_by,
            self.having,
        )
    }
}

impl<'a, S, D, W, O, LOf, G> BoxedDsl<'a>
    for SelectStatement<NoFromClause, S, D, W, O, LOf, G>
where
    Self: AsQuery,
    S: SelectClauseExpression<NoFromClause> + QueryFragment + Send + 'a,
    D: QueryFragment + Send + 'a,
    W: Into<BoxedWhereClause<'a>>,
    O: Into<Option<Box<dyn QueryFragment + Send + 'a>>>,
    LOf: IntoBoxedClause<'a, BoxedClause = BoxedLimitOffsetClause<'a>>,
    G: ValidGroupByClause + QueryFragment + Send + 'a,
{
    type Output =
        BoxedSelectStatement<'a, S::SelectClauseSqlType, NoFromClause, G::Expressions>;

    fn internal_into_boxed(self) -> Self::Output {
        BoxedSelectStatement::new_no_from_clause(
            self.select,
            self.from,
            Box::new(self.distinct),
            self.where_clause.into(),
            self.order.into(),
            self.limit_offset.into_boxed(),
            self.group_by,
            self.having,
        )
    }
}

impl<F, S, D, W, O, LOf, G> HasTable
    for SelectStatement<FromClause<F>, S, D, W, O, LOf, G>
where
    F: HasTable + QuerySource,
{
    type Table = F::Table;

    fn table() -> Self::Table {
        F::table()
    }
}

impl<F, W> IntoUpdateTarget
    for SelectStatement<FromClause<F>, DefaultSelectClause<FromClause<F>>, NoDistinctClause, W>
where
    F: QuerySource,
    Self: HasTable,
    W: ValidWhereClause<F>,
{
    type WhereClause = W;

    fn into_update_target(self) -> UpdateTarget<Self::Table, Self::WhereClause> {
        UpdateTarget {
            table: Self::table(),
            where_clause: self.where_clause,
        }
    }
}

// FIXME: Should we disable joining when `.group_by` has been called? Are there
// any other query methods where a join no longer has the same semantics as
// joining on just the table?
impl<F, S, D, W, O, LOf, G, Rhs> JoinTo<Rhs>
    for SelectStatement<FromClause<F>, S, D, W, O, LOf, G>
where
    F: JoinTo<Rhs> + QuerySource,
{
    type FromClause = <F as JoinTo<Rhs>>::FromClause;
    type OnClause = F::OnClause;

    fn join_target(rhs: Rhs) -> (Self::FromClause, Self::OnClause) {
        F::join_target(rhs)
    }
}

impl<F, S, D, W, O, LOf, G> QueryDsl for SelectStatement<F, S, D, W, O, LOf, G > {}

impl<F, S, D, W, O, LOf, G, Conn> RunQueryDsl<Conn>
    for SelectStatement<F, S, D, W, O, LOf, G>
{
}

impl<F, S, D, W, O, LOf, G, Tab> Insertable<Tab>
    for SelectStatement<F, S, D, W, O, LOf, G>
where
    Tab: Table,
    Self: Query,
{
    type Values = InsertFromSelect<Self, Tab::AllColumns>;

    fn values(self) -> Self::Values {
        InsertFromSelect::new(self)
    }
}

impl<'a, F, S, D, W, O, LOf, G, Tab> Insertable<Tab>
    for &'a SelectStatement<F, S, D, W, O, LOf, G>
where
    Tab: Table,
    Self: Query,
{
    type Values = InsertFromSelect<Self, Tab::AllColumns>;

    fn values(self) -> Self::Values {
        InsertFromSelect::new(self)
    }
}

impl<F, S, D, W, O, LOf, G> SelectNullableDsl
    for SelectStatement<F, SelectClause<S>, D, W, O, LOf, G>
{
    type Output = SelectStatement<F, SelectClause<Nullable<S>>, D, W, O, LOf, G>;

    fn nullable(self) -> Self::Output {
        SelectStatement::new(
            SelectClause(Nullable::new(self.select.0)),
            self.from,
            self.distinct,
            self.where_clause,
            self.order,
            self.limit_offset,
            self.group_by,
            self.having,
            self.locking,
        )
    }
}

impl<F, D, W, O, LOf, G> SelectNullableDsl
    for SelectStatement<F, DefaultSelectClause<F>, D, W, O, LOf, G>
where
    F: AsQuerySource,
{
    type Output = SelectStatement<
        F,
        SelectClause<Nullable<<F::QuerySource as QuerySource>::DefaultSelection>>,
        D,
        W,
        O,
        LOf,
        G,
    >;

    fn nullable(self) -> Self::Output {
        SelectStatement::new(
            SelectClause(Nullable::new(
                self.from.as_query_source().default_selection(),
            )),
            self.from,
            self.distinct,
            self.where_clause,
            self.order,
            self.limit_offset,
            self.group_by,
            self.having,
            self.locking,
        )
    }
}

impl<F, S, D, W, O, LOf, G, Predicate> HavingDsl<Predicate>
    for SelectStatement<F, S, D, W, O, LOf, GroupByClause<G>>
where
    Predicate: AppearsInQuery<F>,
    Predicate: Expression,
    Predicate: QueryFragment,
    Predicate: Send + 'static,
    Predicate::SqlType: BoolOrNullableBool,
{
    type Output = SelectStatement<F, S, D, W, O, LOf, GroupByClause<G>>;

    fn having(self, predicate: Predicate) -> Self::Output {
        SelectStatement::new(
            self.select,
            self.from,
            self.distinct,
            self.where_clause,
            self.order,
            self.limit_offset,
            self.group_by,
            HavingClause::new(predicate),
            self.locking,
        )
    }
}

impl<F, S, D, W, O, LOf, G> CombineDsl for SelectStatement<F, S, D, W, O, LOf, G>
where
    Self: Query,
{
    type Query = Self;

    fn union<Rhs>(self, rhs: Rhs) -> crate::dsl::Union<Self, Rhs>
    where
        Rhs: AsQuery<SqlType = <Self::Query as Query>::SqlType>,
    {
        CombinationClause::new(Union, Distinct, self, rhs.as_query())
    }

    fn union_all<Rhs>(self, rhs: Rhs) -> crate::dsl::UnionAll<Self, Rhs>
    where
        Rhs: AsQuery<SqlType = <Self::Query as Query>::SqlType>,
    {
        CombinationClause::new(Union, All, self, rhs.as_query())
    }

    fn intersect<Rhs>(self, rhs: Rhs) -> crate::dsl::Intersect<Self, Rhs>
    where
        Rhs: AsQuery<SqlType = <Self::Query as Query>::SqlType>,
    {
        CombinationClause::new(Intersect, Distinct, self, rhs.as_query())
    }

    fn intersect_all<Rhs>(self, rhs: Rhs) -> crate::dsl::IntersectAll<Self, Rhs>
    where
        Rhs: AsQuery<SqlType = <Self::Query as Query>::SqlType>,
    {
        CombinationClause::new(Intersect, All, self, rhs.as_query())
    }

    fn except<Rhs>(self, rhs: Rhs) -> crate::dsl::Except<Self, Rhs>
    where
        Rhs: AsQuery<SqlType = <Self::Query as Query>::SqlType>,
    {
        CombinationClause::new(Except, Distinct, self, rhs.as_query())
    }

    fn except_all<Rhs>(self, rhs: Rhs) -> crate::dsl::ExceptAll<Self, Rhs>
    where
        Rhs: AsQuery<SqlType = <Self::Query as Query>::SqlType>,
    {
        CombinationClause::new(Except, All, self, rhs.as_query())
    }
}
