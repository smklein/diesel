use crate::query_builder::*;
use crate::query_source::UntypedColumn;
use crate::result::QueryResult;

/// Represents the column list for use in an insert statement.
///
/// This trait is implemented by columns and tuples of columns.
pub trait ColumnList {
    /// Generate the SQL for this column list.
    fn walk_ast(&self, out: AstPass<'_, '_>) -> QueryResult<()>;
}

impl<C> ColumnList for C
where
    C: UntypedColumn,
{
    fn walk_ast(&self, mut out: AstPass<'_, '_>) -> QueryResult<()> {
        UntypedColumn::walk_ast(self, out.reborrow())
    }
}
