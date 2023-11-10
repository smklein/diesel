use super::field_alias_mapper::FieldAliasMapper;
use super::{Alias, AliasSource};

use crate::dsl;
#[cfg(feature = "postgres_backend")]
use crate::expression::SelectableExpression;
use crate::expression::{Expression, TypedExpressionType};
use crate::expression_methods::EqAll;
use crate::query_builder::{combination_clause, AsQuery, FromClause, Query, SelectStatement};
use crate::query_builder::locking_clause::AllLockingClauses;
use crate::query_dsl::methods::*;
use crate::query_dsl::{CombineDsl, QueryDsl, RunQueryDsl};
use crate::query_source::{QuerySource, Table};

impl<S: AliasSource> QueryDsl for Alias<S> {}

impl<S, Predicate> FilterDsl<Predicate> for Alias<S>
where
    Self: AsQuery,
    <Self as AsQuery>::Query: FilterDsl<Predicate>,
{
    type Output = dsl::Filter<<Self as AsQuery>::Query, Predicate>;

    fn filter(self, predicate: Predicate) -> Self::Output {
        self.as_query().filter(predicate)
    }
}

impl<S, Selection> SelectDsl<Selection> for Alias<S>
where
    Selection: Expression,
    Self: AsQuery,
    <Self as AsQuery>::Query: SelectDsl<Selection>,
{
    type Output = dsl::Select<<Self as AsQuery>::Query, Selection>;

    fn select(self, selection: Selection) -> Self::Output {
        self.as_query().select(selection)
    }
}

impl<S, PK> FindDsl<PK> for Alias<S>
where
    S: AliasSource,
    S::Target: Table,
    <S::Target as Table>::PrimaryKey: FieldAliasMapper<S>,
    <<S::Target as Table>::PrimaryKey as FieldAliasMapper<S>>::Out: EqAll<PK>,
    Self: FilterDsl<
        <<<S::Target as Table>::PrimaryKey as FieldAliasMapper<S>>::Out as EqAll<PK>>::Output,
    >,
{
    type Output = dsl::Filter<
        Self,
        <<<S::Target as Table>::PrimaryKey as FieldAliasMapper<S>>::Out as EqAll<PK>>::Output,
    >;

    fn find(self, id: PK) -> Self::Output {
        let primary_key = self.source.target().primary_key();
        let predicate = self.fields(primary_key).eq_all(id);
        QueryDsl::filter(self, predicate)
    }
}

impl<'a, S> BoxedDsl<'a> for Alias<S>
where
    Alias<S>: QuerySource + AsQuery<Query = SelectStatement<'a, FromClause<Alias<S>>>>,
    SelectStatement<'a, FromClause<Alias<S>>>: BoxedDsl<'a>,
    <Alias<S> as QuerySource>::DefaultSelection:
        Expression<SqlType = <Alias<S> as AsQuery>::SqlType>,
    <Alias<S> as AsQuery>::SqlType: TypedExpressionType,
{
    type Output = dsl::IntoBoxed<'a, SelectStatement<'a, FromClause<Alias<S>>>>;

    fn internal_into_boxed(self) -> Self::Output {
        self.as_query().internal_into_boxed()
    }
}

impl<S> CombineDsl for Alias<S>
where S: AliasSource,
    S::Target: Table,
    Self: AsQuery,
{
    type Query = <Self as AsQuery>::Query;

    fn union<Rhs>(self, rhs: Rhs) -> dsl::Union<Self, Rhs>
    where
        Rhs: AsQuery<SqlType = <<Self as AsQuery>::Query as Query>::SqlType>,
    {
        combination_clause::CombinationClause::new(
            combination_clause::Union,
            combination_clause::Distinct,
            self.as_query(),
            rhs.as_query(),
        )
    }

    fn union_all<Rhs>(self, rhs: Rhs) -> dsl::UnionAll<Self, Rhs>
    where
        Rhs: AsQuery<SqlType = <<Self as AsQuery>::Query as Query>::SqlType>,
    {
        combination_clause::CombinationClause::new(
            combination_clause::Union,
            combination_clause::All,
            self.as_query(),
            rhs.as_query(),
        )
    }

    fn intersect<Rhs>(self, rhs: Rhs) -> dsl::Intersect<Self, Rhs>
    where
        Rhs: AsQuery<SqlType = <<Self as AsQuery>::Query as Query>::SqlType>,
    {
        combination_clause::CombinationClause::new(
            combination_clause::Intersect,
            combination_clause::Distinct,
            self.as_query(),
            rhs.as_query(),
        )
    }

    fn intersect_all<Rhs>(self, rhs: Rhs) -> dsl::IntersectAll<Self, Rhs>
    where
        Rhs: AsQuery<SqlType = <<Self as AsQuery>::Query as Query>::SqlType>,
    {
        combination_clause::CombinationClause::new(
            combination_clause::Intersect,
            combination_clause::All,
            self.as_query(),
            rhs.as_query(),
        )
    }

    fn except<Rhs>(self, rhs: Rhs) -> dsl::Except<Self, Rhs>
    where
        Rhs: AsQuery<SqlType = <<Self as AsQuery>::Query as Query>::SqlType>,
    {
        combination_clause::CombinationClause::new(
            combination_clause::Except,
            combination_clause::Distinct,
            self.as_query(),
            rhs.as_query(),
        )
    }

    fn except_all<Rhs>(self, rhs: Rhs) -> dsl::ExceptAll<Self, Rhs>
    where
        Rhs: AsQuery<SqlType = <<Self as AsQuery>::Query as Query>::SqlType>,
    {
        combination_clause::CombinationClause::new(
            combination_clause::Except,
            combination_clause::All,
            self.as_query(),
            rhs.as_query(),
        )
    }
}

#[cfg(feature = "postgres_backend")]
impl<'a, S, Selection> DistinctOnDsl<Selection> for Alias<S>
where
    S: AliasSource,
    Selection: SelectableExpression<Self>,
    Self: QuerySource + AsQuery<Query = SelectStatement<'a, FromClause<Self>>>,
    SelectStatement<'a, FromClause<Self>>: DistinctOnDsl<Selection>,
    <Self as QuerySource>::DefaultSelection:
        Expression<SqlType = <Self as AsQuery>::SqlType>,
    <Self as AsQuery>::SqlType: TypedExpressionType,
{
    type Output = dsl::DistinctOn<SelectStatement<'a, FromClause<Self>>, Selection>;

    fn distinct_on(self, selection: Selection) -> dsl::DistinctOn<Self, Selection> {
        DistinctOnDsl::distinct_on(self.as_query(), selection)
    }
}

impl<S, Predicate> OrFilterDsl<Predicate> for Alias<S>
where
    Self: AsQuery,
    <Self as AsQuery>::Query: OrFilterDsl<Predicate>,
{
    type Output = dsl::OrFilter<<Self as AsQuery>::Query, Predicate>;

    fn or_filter(self, predicate: Predicate) -> Self::Output {
        self.as_query().or_filter(predicate)
    }
}

impl<S> LimitDsl for Alias<S>
where
    Self: AsQuery,
    <Self as AsQuery>::Query: LimitDsl,
{
    type Output = <<Self as AsQuery>::Query as LimitDsl>::Output;

    fn limit(self, limit: i64) -> Self::Output {
        self.as_query().limit(limit)
    }
}

impl<'a, S> LockingDsl for Alias<S>
where
    Self: QuerySource + AsQuery<Query = SelectStatement<'a, FromClause<Self>>>,
    <Self as QuerySource>::DefaultSelection: Expression<SqlType = <Self as AsQuery>::SqlType>,
    <Self as AsQuery>::SqlType: TypedExpressionType,
{
    type Output = <SelectStatement<'a, FromClause<Self>> as LockingDsl>::Output;

    fn with_lock(self, lock: AllLockingClauses) -> Self::Output {
        crate::query_dsl::QueryDsl::with_lock(self.as_query(), lock)
//        locking_dsl::LockingDsl::with_lock(self.as_query(), lock)
//        self.as_query().with_lock(lock)
    }
}

impl<S: AliasSource, Conn> RunQueryDsl<Conn> for Alias<S> {}

impl<S> OffsetDsl for Alias<S>
where
    Self: AsQuery,
    <Self as AsQuery>::Query: OffsetDsl,
{
    type Output = <<Self as AsQuery>::Query as OffsetDsl>::Output;

    fn offset(self, offset: i64) -> Self::Output {
        self.as_query().offset(offset)
    }
}

impl<S, Expr> OrderDsl<Expr> for Alias<S>
where
    Expr: Expression,
    Self: AsQuery,
    <Self as AsQuery>::Query: OrderDsl<Expr>,
{
    type Output = <<Self as AsQuery>::Query as OrderDsl<Expr>>::Output;

    fn order(self, expr: Expr) -> Self::Output {
        self.as_query().order(expr)
    }
}

impl<S, Expr> ThenOrderDsl<Expr> for Alias<S>
where
    Expr: Expression,
    Self: AsQuery,
    <Self as AsQuery>::Query: ThenOrderDsl<Expr>,
{
    type Output = <<Self as AsQuery>::Query as ThenOrderDsl<Expr>>::Output;

    fn then_order_by(self, expr: Expr) -> Self::Output {
        self.as_query().then_order_by(expr)
    }
}
