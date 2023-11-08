use crate::expression::operators::Concat;
use crate::mysql::backend::MysqlOnConflictClause;
use crate::mysql::Mysql;
use crate::query_builder::insert_statement::DefaultValues;
use crate::query_builder::nodes::StaticQueryFragment;
use crate::query_builder::upsert::into_conflict_clause::OnConflictSelectWrapper;
use crate::query_builder::upsert::on_conflict_actions::{DoNothing, DoUpdate};
use crate::query_builder::upsert::on_conflict_clause::OnConflictValues;
use crate::query_builder::upsert::on_conflict_target::{ConflictTarget, OnConflictTarget};
use crate::query_builder::where_clause::NoWhereClause;
use crate::query_builder::{AstPass, DB, QueryFragment};
use crate::result::QueryResult;
use crate::{Column, Table};

impl QueryFragment<crate::mysql::backend::MysqlStyleDefaultValueClause> for DefaultValues {
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b, Mysql>) -> QueryResult<()> {
        out.push_sql("() VALUES ()");
        Ok(())
    }
}

impl<L, R> QueryFragment<crate::mysql::backend::MysqlConcatClause> for Concat<L, R>
where
    L: QueryFragment,
    R: QueryFragment,
{
    fn walk_ast<'b>(
        &'b self,
        mut out: crate::query_builder::AstPass<'_, 'b, Mysql>,
    ) -> crate::result::QueryResult<()> {
        out.push_sql("CONCAT(");
        self.left.walk_ast(out.reborrow())?;
        out.push_sql(",");
        self.right.walk_ast(out.reborrow())?;
        out.push_sql(")");
        Ok(())
    }
}

impl<T> QueryFragment<crate::mysql::backend::MysqlOnConflictClause> for DoNothing<T>
where
    T: Table + StaticQueryFragment,
    T::Component: QueryFragment,
    T::PrimaryKey: Column,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b, Mysql>) -> QueryResult<()> {
        out.push_sql(" UPDATE ");
        T::STATIC_COMPONENT.walk_ast(out.reborrow())?;
        out.push_sql(".");
        out.push_identifier(<T::PrimaryKey as Column>::NAME)?;
        out.push_sql(" = ");
        T::STATIC_COMPONENT.walk_ast(out.reborrow())?;
        out.push_sql(".");
        out.push_identifier(<T::PrimaryKey as Column>::NAME)?;
        Ok(())
    }
}

impl<T, Tab> QueryFragment<crate::mysql::backend::MysqlOnConflictClause> for DoUpdate<T, Tab>
where
    T: QueryFragment,
    Tab: Table + StaticQueryFragment,
    Tab::Component: QueryFragment,
    Tab::PrimaryKey: Column,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b, Mysql>) -> QueryResult<()> {
        out.unsafe_to_cache_prepared();
        out.push_sql(" UPDATE ");
        if self.changeset.is_noop(out.backend())? {
            Tab::STATIC_COMPONENT.walk_ast(out.reborrow())?;
            out.push_sql(".");
            out.push_identifier(<Tab::PrimaryKey as Column>::NAME)?;
            out.push_sql(" = ");
            Tab::STATIC_COMPONENT.walk_ast(out.reborrow())?;
            out.push_sql(".");
            out.push_identifier(<Tab::PrimaryKey as Column>::NAME)?;
        } else {
            self.changeset.walk_ast(out.reborrow())?;
        }
        Ok(())
    }
}

impl<Values, Target, Action> QueryFragment<MysqlOnConflictClause>
    for OnConflictValues<Values, Target, Action, NoWhereClause>
where
    Values: QueryFragment,
    Target: QueryFragment,
    Action: QueryFragment,
    NoWhereClause: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b, Mysql>) -> QueryResult<()> {
        self.values.walk_ast(out.reborrow())?;
        out.push_sql(" ON DUPLICATE KEY");
        self.target.walk_ast(out.reborrow())?;
        self.action.walk_ast(out.reborrow())?;
        self.where_clause.walk_ast(out)?;
        Ok(())
    }
}

/// A marker type signaling that the given `ON CONFLICT` clause
/// uses mysql's `ON DUPLICATE KEY` syntax that triggers on
/// all unique constraints
///
/// See [`InsertStatement::on_conflict`](crate::query_builder::InsertStatement::on_conflict)
/// for examples
#[derive(Debug, Copy, Clone)]
pub struct DuplicatedKeys;

impl<Tab> OnConflictTarget<Tab> for ConflictTarget<DuplicatedKeys> {}

impl QueryFragment<MysqlOnConflictClause> for ConflictTarget<DuplicatedKeys> {
    fn walk_ast<'b>(&'b self, _out: AstPass<'_, 'b, Mysql>) -> QueryResult<()> {
        Ok(())
    }
}

impl<S> QueryFragment for OnConflictSelectWrapper<S>
where
    S: QueryFragment,
{
    fn walk_ast<'b>(&'b self, out: AstPass<'_, 'b, crate::mysql::Mysql>) -> QueryResult<()> {
        self.0.walk_ast(out)
    }
}
