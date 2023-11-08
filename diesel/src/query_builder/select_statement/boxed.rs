use std::marker::PhantomData;

use crate::backend::{sql_dialect, SqlDialect};
use crate::dsl::AsExprOf;
use crate::expression::subselect::ValidSubselect;
use crate::expression::*;
use crate::insertable::Insertable;
use crate::query_builder::combination_clause::*;
use crate::query_builder::distinct_clause::DistinctClause;
use crate::query_builder::from_clause::FromClause;
use crate::query_builder::group_by_clause::ValidGroupByClause;
use crate::query_builder::having_clause::HavingClause;
use crate::query_builder::insert_statement::InsertFromSelect;
use crate::query_builder::limit_clause::LimitClause;
use crate::query_builder::limit_offset_clause::BoxedLimitOffsetClause;
use crate::query_builder::offset_clause::OffsetClause;
use crate::query_builder::order_clause::OrderClause;
use crate::query_builder::where_clause::{BoxedWhereClause, WhereAnd, WhereOr};
use crate::query_builder::*;
use crate::query_dsl::methods::*;
use crate::query_dsl::*;
use crate::query_source::joins::*;
use crate::query_source::{QuerySource, Table};
use crate::result::QueryResult;
use crate::sql_types::{BigInt, BoolOrNullableBool, IntoNullable};

// This is used by the table macro internally
/// This type represents a boxed select query
///
/// Using this type directly is only meaningful for custom backends
/// that need to provide a custom [`QueryFragment`] implementation
#[allow(missing_debug_implementations)]
#[must_use = "Queries are only executed when calling `load`, `get_result` or similar."]
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
        having
    )
)]
pub struct BoxedSelectStatement<'a, ST, QS, GB = ()> {
    /// The select clause of the query
    select: Box<dyn QueryFragment + Send + 'a>,
    /// The from clause of the query
    from: QS,
    /// The distinct clause of the query
    distinct: Box<dyn QueryFragment + Send + 'a>,
    /// The where clause of the query
    where_clause: BoxedWhereClause<'a>,
    /// The order clause of the query
    order: Option<Box<dyn QueryFragment + Send + 'a>>,
    /// The combined limit/offset clause of the query
    limit_offset: BoxedLimitOffsetClause<'a>,
    /// The group by clause of the query
    group_by: Box<dyn QueryFragment + Send + 'a>,
    /// The having clause of the query
    having: HavingClause,
    _marker: PhantomData<(ST, GB)>,
}

impl<'a, ST, QS: QuerySource, GB> BoxedSelectStatement<'a, ST, FromClause<QS>, GB> {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn new<S, G>(
        select: S,
        from: FromClause<QS>,
        distinct: Box<dyn QueryFragment + Send + 'a>,
        where_clause: BoxedWhereClause<'a>,
        order: Option<Box<dyn QueryFragment + Send + 'a>>,
        limit_offset: BoxedLimitOffsetClause<'a>,
        group_by: G,
        having: HavingClause,
    ) -> Self
    where
        G: ValidGroupByClause<Expressions = GB> + QueryFragment + Send + 'a,
        S: SelectClauseExpression<FromClause<QS>, SelectClauseSqlType = ST>
            + QueryFragment
            + Send
            + 'a,
    {
        BoxedSelectStatement {
            select: Box::new(select),
            from,
            distinct,
            where_clause,
            order,
            limit_offset,
            group_by: Box::new(group_by),
            having,
            _marker: PhantomData,
        }
    }
}

impl<'a, ST, GB> BoxedSelectStatement<'a, ST, NoFromClause, GB> {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn new_no_from_clause<S, G>(
        select: S,
        from: NoFromClause,
        distinct: Box<dyn QueryFragment + Send + 'a>,
        where_clause: BoxedWhereClause<'a>,
        order: Option<Box<dyn QueryFragment + Send + 'a>>,
        limit_offset: BoxedLimitOffsetClause<'a>,
        group_by: G,
        having: HavingClause,
    ) -> Self
    where
        G: ValidGroupByClause<Expressions = GB> + QueryFragment + Send + 'a,
        S: SelectClauseExpression<NoFromClause, SelectClauseSqlType = ST>
            + QueryFragment
            + Send
            + 'a,
    {
        BoxedSelectStatement {
            select: Box::new(select),
            from,
            distinct,
            where_clause,
            order,
            limit_offset,
            group_by: Box::new(group_by),
            having,
            _marker: PhantomData,
        }
    }
}

// that's a trait to control who can access these methods
#[doc(hidden)] // exported via internal::derives::multiconnection
pub trait BoxedQueryHelper<'a, QS> {
    fn build_query<'b, 'c>(
        &'b self,
        out: AstPass<'_, 'c, DB>,
        where_clause_handler: impl Fn(
            &'b BoxedWhereClause<'a>,
            AstPass<'_, 'c, DB>,
        ) -> QueryResult<()>,
    ) -> QueryResult<()>
    where
        QS: QueryFragment,
        BoxedLimitOffsetClause<'a>: QueryFragment,
        'b: 'c;
}

impl<'a, ST, QS, GB> BoxedQueryHelper<'a, QS> for BoxedSelectStatement<'a, ST, QS, GB> {
    fn build_query<'b, 'c>(
        &'b self,
        mut out: AstPass<'_, 'c, DB>,
        where_clause_handler: impl Fn(
            &'b BoxedWhereClause<'a>,
            AstPass<'_, 'c, DB>,
        ) -> QueryResult<()>,
    ) -> QueryResult<()>
    where
        QS: QueryFragment,
        BoxedLimitOffsetClause<'a>: QueryFragment,
        'b: 'c,
    {
        out.push_sql("SELECT ");
        self.distinct.walk_ast(out.reborrow())?;
        self.select.walk_ast(out.reborrow())?;
        self.from.walk_ast(out.reborrow())?;
        where_clause_handler(&self.where_clause, out.reborrow())?;
        self.group_by.walk_ast(out.reborrow())?;
        self.having.walk_ast(out.reborrow())?;

        if let Some(ref order) = self.order {
            out.push_sql(" ORDER BY ");
            order.walk_ast(out.reborrow())?;
        }
        self.limit_offset.walk_ast(out.reborrow())?;
        Ok(())
    }
}

impl<'a, ST, QS, GB> Query for BoxedSelectStatement<'a, ST, QS, GB> {
    type SqlType = ST;
}

impl<'a, ST, QS, GB> SelectQuery for BoxedSelectStatement<'a, ST, QS, GB> {
    type SqlType = ST;
}

impl<'a, ST, QS, QS2, GB> ValidSubselect<QS2> for BoxedSelectStatement<'a, ST, QS, GB> where
    Self: Query<SqlType = ST>
{
}

impl<'a, ST, QS, GB> QueryFragment for BoxedSelectStatement<'a, ST, QS, GB>
where
    Self: QueryFragment<<DB as SqlDialect>::SelectStatementSyntax>,
{
    fn walk_ast<'b>(&'b self, pass: AstPass<'_, 'b, DB>) -> QueryResult<()> {
        <Self as QueryFragment<DB::SelectStatementSyntax>>::walk_ast(self, pass)
    }
}

impl<'a, ST, QS, GB>
    QueryFragment<sql_dialect::select_statement_syntax::AnsiSqlSelectStatement>
    for BoxedSelectStatement<'a, ST, QS, GB>
where
    QS: QueryFragment,
    BoxedLimitOffsetClause<'a>: QueryFragment,
{
    fn walk_ast<'b>(&'b self, out: AstPass<'_, 'b, DB>) -> QueryResult<()> {
        self.build_query(out, |where_clause, out| where_clause.walk_ast(out))
    }
}

impl<'a, ST, QS, GB> QueryId for BoxedSelectStatement<'a, ST, QS, GB> {
    type QueryId = ();

    const HAS_STATIC_QUERY_ID: bool = false;
}

impl<'a, ST, QS, Rhs, Kind, On, GB> InternalJoinDsl<Rhs, Kind, On>
    for BoxedSelectStatement<'a, ST, FromClause<QS>, GB>
where
    QS: QuerySource,
    Rhs: QuerySource,
    JoinOn<Join<QS, Rhs, Kind>, On>: QuerySource, BoxedSelectStatement<'a, ST, FromClause<JoinOn<Join<QS, Rhs, Kind>, On>>, GB>: AsQuery,
{
    type Output = BoxedSelectStatement<'a, ST, FromClause<JoinOn<Join<QS, Rhs, Kind>, On>>, GB>;

    fn join(self, rhs: Rhs, kind: Kind, on: On) -> Self::Output {
        BoxedSelectStatement {
            select: self.select,
            from: FromClause::new(Join::new(self.from.source, rhs, kind).on(on)),
            distinct: self.distinct,
            where_clause: self.where_clause,
            order: self.order,
            limit_offset: self.limit_offset,
            group_by: self.group_by,
            having: self.having,
            _marker: PhantomData,
        }
    }
}

impl<'a, ST, QS, GB> DistinctDsl for BoxedSelectStatement<'a, ST, QS, GB>
where
    DistinctClause: QueryFragment,
{
    type Output = Self;

    fn distinct(mut self) -> Self::Output {
        self.distinct = Box::new(DistinctClause);
        self
    }
}

impl<'a, ST, QS, Selection, GB> SelectDsl<Selection>
    for BoxedSelectStatement<'a, ST, FromClause<QS>, GB>
where
    QS: QuerySource,
    Selection: SelectableExpression<QS> + QueryFragment + Send + 'a,
{
    type Output = BoxedSelectStatement<'a, Selection::SqlType, FromClause<QS>, GB>;

    fn select(self, selection: Selection) -> Self::Output {
        BoxedSelectStatement {
            select: Box::new(selection),
            from: self.from,
            distinct: self.distinct,
            where_clause: self.where_clause,
            order: self.order,
            limit_offset: self.limit_offset,
            group_by: self.group_by,
            having: self.having,
            _marker: PhantomData,
        }
    }
}

impl<'a, ST, Selection, GB> SelectDsl<Selection>
    for BoxedSelectStatement<'a, ST, NoFromClause, GB>
where
    Selection:
        SelectableExpression<NoFromClause> + QueryFragment + Send + 'a,
{
    type Output = BoxedSelectStatement<'a, Selection::SqlType, NoFromClause, GB>;

    fn select(self, selection: Selection) -> Self::Output {
        BoxedSelectStatement {
            select: Box::new(selection),
            from: self.from,
            distinct: self.distinct,
            where_clause: self.where_clause,
            order: self.order,
            limit_offset: self.limit_offset,
            group_by: self.group_by,
            having: self.having,
            _marker: PhantomData,
        }
    }
}

impl<'a, ST, QS, Predicate, GB> FilterDsl<Predicate>
    for BoxedSelectStatement<'a, ST, FromClause<QS>, GB>
where
    QS: QuerySource,
    BoxedWhereClause<'a>: WhereAnd<Predicate, Output = BoxedWhereClause<'a>>,
    Predicate: AppearsInQuery<QS>,
    Predicate::SqlType: BoolOrNullableBool,
{
    type Output = Self;

    fn filter(mut self, predicate: Predicate) -> Self::Output {
        self.where_clause = self.where_clause.and(predicate);
        self
    }
}

impl<'a, ST, Predicate, GB> FilterDsl<Predicate>
    for BoxedSelectStatement<'a, ST, NoFromClause, GB>
where
    BoxedWhereClause<'a>: WhereAnd<Predicate, Output = BoxedWhereClause<'a>>,
    Predicate: AppearsInQuery<NoFromClause>,
    Predicate::SqlType: BoolOrNullableBool,
{
    type Output = Self;

    fn filter(mut self, predicate: Predicate) -> Self::Output {
        self.where_clause = self.where_clause.and(predicate);
        self
    }
}

impl<'a, ST, QS, Predicate, GB> OrFilterDsl<Predicate>
    for BoxedSelectStatement<'a, ST, FromClause<QS>, GB>
where
    QS: QuerySource,
    BoxedWhereClause<'a>: WhereOr<Predicate, Output = BoxedWhereClause<'a>>,
    Predicate: AppearsInQuery<QS>,
    Predicate::SqlType: BoolOrNullableBool,
{
    type Output = Self;

    fn or_filter(mut self, predicate: Predicate) -> Self::Output {
        self.where_clause = self.where_clause.or(predicate);
        self
    }
}

impl<'a, ST, Predicate, GB> OrFilterDsl<Predicate>
    for BoxedSelectStatement<'a, ST, NoFromClause, GB>
where
    BoxedWhereClause<'a>: WhereOr<Predicate, Output = BoxedWhereClause<'a>>,
    Predicate: AppearsInQuery<NoFromClause>,
    Predicate::SqlType: BoolOrNullableBool,
{
    type Output = Self;

    fn or_filter(mut self, predicate: Predicate) -> Self::Output {
        self.where_clause = self.where_clause.or(predicate);
        self
    }
}

impl<'a, ST, QS, GB> LimitDsl for BoxedSelectStatement<'a, ST, QS, GB> where
    LimitClause<AsExprOf<i64, BigInt>>: QueryFragment,
{
    type Output = Self;

    fn limit(mut self, limit: i64) -> Self::Output {
        self.limit_offset.limit = Some(Box::new(LimitClause(limit.into_sql::<BigInt>())));
        self
    }
}

impl<'a, ST, QS, GB> OffsetDsl for BoxedSelectStatement<'a, ST, QS, GB>
where
    OffsetClause<AsExprOf<i64, BigInt>>: QueryFragment,
{
    type Output = Self;

    fn offset(mut self, offset: i64) -> Self::Output {
        self.limit_offset.offset = Some(Box::new(OffsetClause(offset.into_sql::<BigInt>())));
        self
    }
}

// no impls for `NoFromClause` here because order is not really supported there yet
impl<'a, ST, QS, Order, GB> OrderDsl<Order>
    for BoxedSelectStatement<'a, ST, FromClause<QS>, GB>
where
    QS: QuerySource,
    Order: QueryFragment + AppearsInQuery<QS> + Send + 'a,
{
    type Output = Self;

    fn order(mut self, order: Order) -> Self::Output {
        self.order = OrderClause(order).into();
        self
    }
}

impl<'a, ST, QS, Order, GB> ThenOrderDsl<Order>
    for BoxedSelectStatement<'a, ST, FromClause<QS>, GB>
where
    QS: QuerySource,
    Order: QueryFragment + AppearsInQuery<QS> + Send + 'a,
{
    type Output = Self;

    fn then_order_by(mut self, order: Order) -> Self::Output {
        self.order = match self.order {
            Some(old) => Some(Box::new((old, order))),
            None => Some(Box::new(order)),
        };
        self
    }
}

impl<'a, ST, QS, Rhs> JoinTo<Rhs> for BoxedSelectStatement<'a, ST, FromClause<QS>, ()>
where
    QS: JoinTo<Rhs> + QuerySource,
{
    type FromClause = <QS as JoinTo<Rhs>>::FromClause;
    type OnClause = QS::OnClause;

    fn join_target(rhs: Rhs) -> (Self::FromClause, Self::OnClause) {
        QS::join_target(rhs)
    }
}

impl<'a, ST, QS, GB> QueryDsl for BoxedSelectStatement<'a, ST, QS, GB> {}

impl<'a, ST, QS, Conn, GB> RunQueryDsl<Conn> for BoxedSelectStatement<'a, ST, QS, GB> {}

impl<'a, ST, QS, T, GB> Insertable<T> for BoxedSelectStatement<'a, ST, QS, GB>
where
    T: Table,
    Self: Query,
{
    type Values = InsertFromSelect<Self, T::AllColumns>;

    fn values(self) -> Self::Values {
        InsertFromSelect::new(self)
    }
}

impl<'a, 'b, ST, QS, T, GB> Insertable<T> for &'b BoxedSelectStatement<'a, ST, QS, GB>
where
    T: Table,
    Self: Query,
{
    type Values = InsertFromSelect<Self, T::AllColumns>;

    fn values(self) -> Self::Values {
        InsertFromSelect::new(self)
    }
}

impl<'a, ST, QS, GB> SelectNullableDsl for BoxedSelectStatement<'a, ST, QS, GB>
where
    ST: IntoNullable,
{
    type Output = BoxedSelectStatement<'a, ST::Nullable, QS>;

    fn nullable(self) -> Self::Output {
        BoxedSelectStatement {
            select: self.select,
            from: self.from,
            distinct: self.distinct,
            where_clause: self.where_clause,
            order: self.order,
            limit_offset: self.limit_offset,
            group_by: self.group_by,
            having: self.having,
            _marker: PhantomData,
        }
    }
}

impl<'a, ST, QS, GB, Predicate> HavingDsl<Predicate>
    for BoxedSelectStatement<'a, ST, QS, GB>
where
    GB: Expression,
    HavingClause: QueryFragment + Send + 'a,
    Predicate: AppearsInQuery<QS>,
    Predicate::SqlType: BoolOrNullableBool,
{
    type Output = Self;

    fn having(mut self, predicate: Predicate) -> Self::Output {
        self.having = HavingClause::new(predicate);
        self
    }
}

impl<'a, ST, QS, GB> CombineDsl for BoxedSelectStatement<'a, ST, QS, GB>
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

#[cfg(test)]
mod tests {
    use crate::prelude::*;

    table! {
        users {
            id -> Integer,
        }
    }

    fn assert_send<T>(_: T)
    where
        T: Send,
    {
    }

    macro_rules! assert_boxed_query_send {
        ($backend:ty) => {{
            assert_send(users::table.into_boxed::<$backend>());
            assert_send(
                users::table
                    .filter(users::id.eq(10))
                    .into_boxed::<$backend>(),
            );
        };};
    }

    #[test]
    fn boxed_is_send() {
        #[cfg(feature = "postgres")]
        assert_boxed_query_send!(crate::pg::Pg);

        #[cfg(feature = "sqlite")]
        assert_boxed_query_send!(crate::sqlite::Sqlite);

        #[cfg(feature = "mysql")]
        assert_boxed_query_send!(crate::mysql::Mysql);
    }
}
