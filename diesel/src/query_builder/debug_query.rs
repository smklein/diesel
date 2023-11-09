use super::{AstPass, DB, QueryFragment};
use std::fmt::{self, Debug, Display};
use crate::query_builder::QueryBuilder as _;

/// A struct that implements `fmt::Display` and `fmt::Debug` to show the SQL
/// representation of a query.
///
/// The `Display` implementation will be the exact query sent to the server,
/// plus a comment with the values of the bind parameters. The `Debug`
/// implementation is more structured, and able to be pretty printed.
///
/// See [`debug_query`] for usage examples.
///
/// [`debug_query`]: crate::query_builder::debug_query()
pub struct DebugQuery<'a, T: 'a> {
    pub(crate) query: &'a T,
}

impl<'a, T> DebugQuery<'a, T> {
    pub(crate) fn new(query: &'a T) -> Self {
        DebugQuery {
            query,
        }
    }
}

type QueryBuilder = <DB as crate::query_builder::Backend>::QueryBuilder;

impl<'a, T> Display for DebugQuery<'a, T>
where
    T: QueryFragment,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut query_builder = QueryBuilder::default();
        let backend = DB::default();
        QueryFragment::to_sql(self.query, &mut query_builder, &backend)
            .map_err(|_| fmt::Error)?;
        let debug_binds = DebugBinds::<_>::new(self.query);
        write!(f, "{} -- binds: {:?}", query_builder.finish(), debug_binds)
    }
}

impl<'a, T> Debug for DebugQuery<'a, T>
where
    T: QueryFragment,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut query_builder = QueryBuilder::default();
        let backend = DB::default();
        QueryFragment::to_sql(self.query, &mut query_builder, &backend)
            .map_err(|_| fmt::Error)?;
        let debug_binds = DebugBinds::new(self.query);
        f.debug_struct("Query")
            .field("sql", &query_builder.finish())
            .field("binds", &debug_binds)
            .finish()
    }
}

/// A struct that implements `fmt::Debug` by walking the given AST and writing
/// the `fmt::Debug` implementation of each bind parameter.
pub(crate) struct DebugBinds<'a, T: 'a> {
    query: &'a T,
}

impl<'a, T> DebugBinds<'a, T> {
    fn new(query: &'a T) -> Self {
        DebugBinds {
            query,
        }
    }
}

impl<'a, T> Debug for DebugBinds<'a, T>
where
    T: QueryFragment,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let backend = DB::default();
        let mut buffer = Vec::new();
        let ast_pass = AstPass::debug_binds(&mut buffer, &backend);
        self.query.walk_ast(ast_pass).map_err(|_| fmt::Error)?;

        let mut list = f.debug_list();
        for entry in buffer {
            list.entry(&entry);
        }
        list.finish()?;
        Ok(())
    }
}
