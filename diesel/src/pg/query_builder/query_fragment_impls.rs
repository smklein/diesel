use crate::expression::array_comparison::{In, Many, MaybeEmpty, NotIn};
use crate::pg::backend::PgStyleArrayComparison;
use crate::pg::types::sql_types::Array;
use crate::pg::Pg;
use crate::query_builder::upsert::into_conflict_clause::OnConflictSelectWrapper;
use crate::query_builder::upsert::on_conflict_target_decorations::DecoratedConflictTarget;
use crate::query_builder::{AstPass, QueryFragment};
use crate::result::QueryResult;
use crate::serialize::ToSql;
use crate::sql_types::{HasSqlType, SingleValue};

impl<T, U> QueryFragment<PgStyleArrayComparison> for In<T, U>
where
    T: QueryFragment,
    U: QueryFragment + MaybeEmpty,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        self.left.walk_ast(out.reborrow())?;
        out.push_sql(" = ANY(");
        self.values.walk_ast(out.reborrow())?;
        out.push_sql(")");
        Ok(())
    }
}

impl<T, U> QueryFragment<PgStyleArrayComparison> for NotIn<T, U>
where
    T: QueryFragment,
    U: QueryFragment + MaybeEmpty,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        self.left.walk_ast(out.reborrow())?;
        out.push_sql(" != ALL(");
        self.values.walk_ast(out.reborrow())?;
        out.push_sql(")");
        Ok(())
    }
}

impl<ST, I> QueryFragment<PgStyleArrayComparison> for Many<ST, I>
where
    ST: SingleValue,
    Vec<I>: ToSql<Array<ST>>,
    Pg: HasSqlType<ST>,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_bind_param::<Array<ST>, Vec<I>>(&self.values)
    }
}

impl<T, U> QueryFragment<crate::pg::backend::PgOnConflictClause>
    for DecoratedConflictTarget<T, U>
where
    T: QueryFragment,
    U: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        self.target.walk_ast(out.reborrow())?;
        self.where_clause.walk_ast(out.reborrow())?;
        Ok(())
    }
}

impl<S> QueryFragment for OnConflictSelectWrapper<S>
where
    S: QueryFragment,
{
    fn walk_ast<'b>(&'b self, out: AstPass<'_, 'b>) -> QueryResult<()> {
        self.0.walk_ast(out)
    }
}
