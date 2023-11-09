use crate::backend::{sql_dialect, Backend, SqlDialect};
use crate::expression::SqlLiteral;
use crate::query_builder::*;
use crate::query_source::Column;
use crate::result::QueryResult;

#[doc(hidden)]
pub trait OnConflictTarget<Table> {}

#[doc(hidden)]
#[derive(Debug, Clone, Copy, QueryId)]
pub struct NoConflictTarget;

impl QueryFragment for NoConflictTarget {
    fn walk_ast<'b>(&'b self, _: AstPass<'_, 'b>) -> QueryResult<()> {
        Ok(())
    }
}

impl<Table> OnConflictTarget<Table> for NoConflictTarget {}

#[doc(hidden)]
#[derive(Debug, Clone, Copy, QueryId)]
pub struct ConflictTarget<T>(pub T);

type OnConflictClause = <DB as SqlDialect>::OnConflictClause;

impl<T> QueryFragment for ConflictTarget<T>
where
    Self: QueryFragment<OnConflictClause>,
{
    fn walk_ast<'b>(&'b self, pass: AstPass<'_, 'b>) -> QueryResult<()> {
        <Self as QueryFragment<OnConflictClause>>::walk_ast(self, pass)
    }
}

impl<T, SP> QueryFragment<SP> for ConflictTarget<T>
where
    DB: Backend<OnConflictClause = SP>,
    SP: sql_dialect::on_conflict_clause::PgLikeOnConflictClause,
    T: Column,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_sql(" (");
        out.push_identifier(T::NAME)?;
        out.push_sql(")");
        Ok(())
    }
}

impl<T> OnConflictTarget<T::Table> for ConflictTarget<T> where T: Column {}

impl<ST, SP> QueryFragment<SP> for ConflictTarget<SqlLiteral<ST>>
where
    DB: Backend<OnConflictClause = SP>,
    SP: sql_dialect::on_conflict_clause::PgLikeOnConflictClause,
    SqlLiteral<ST>: QueryFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_sql(" ");
        self.0.walk_ast(out.reborrow())?;
        Ok(())
    }
}

impl<Tab, ST> OnConflictTarget<Tab> for ConflictTarget<SqlLiteral<ST>> {}

impl<T, SP> QueryFragment<SP> for ConflictTarget<(T,)>
where
    DB: Backend<OnConflictClause = SP>,
    SP: sql_dialect::on_conflict_clause::PgLikeOnConflictClause,
    T: Column,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_sql(" (");
        out.push_identifier(T::NAME)?;
        out.push_sql(")");
        Ok(())
    }
}

impl<T> OnConflictTarget<T::Table> for ConflictTarget<(T,)> where T: Column {}

macro_rules! on_conflict_tuples {
    ($(
        $Tuple:tt {
            $(($idx:tt) -> $T:ident, $ST:ident, $TT:ident,)*
        }
    )+) => {
        $(
            impl<_T, _SP, $($T),*> QueryFragment<_SP> for ConflictTarget<(_T, $($T),*)> where
                DB: Backend<OnConflictClause = _SP>,
                _SP: sql_dialect::on_conflict_clause::PgLikeOnConflictClause,
                _T: Column,
                $($T: Column<Table=_T::Table>,)*
            {
                fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()>
                {
                    out.push_sql(" (");
                    out.push_identifier(_T::NAME)?;
                    $(
                        out.push_sql(", ");
                        out.push_identifier($T::NAME)?;
                    )*
                    out.push_sql(")");
                    Ok(())
                }
            }

            impl<_T, $($T),*> OnConflictTarget<_T::Table> for ConflictTarget<(_T, $($T),*)> where
                _T: Column,
                $($T: Column<Table=_T::Table>,)*
            {
            }
        )*
    }
}

diesel_derives::__diesel_for_each_tuple!(on_conflict_tuples);
