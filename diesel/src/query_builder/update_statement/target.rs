use crate::associations::{HasTable, Identifiable};
use crate::dsl::Find;
use crate::query_dsl::methods::FindDsl;
use crate::query_builder::where_clause::BoxedWhereClause;
use crate::query_source::Table;

#[doc(hidden)]
pub struct UpdateTarget<Table> {
    pub table: Table,
    pub where_clause: BoxedWhereClause<'static>,
}

/// A type which can be passed to [`update`] or [`delete`].
///
/// Apps will never need to implement this type directly. There are three kinds
/// which implement this trait. Tables, queries which have only had `filter`
/// called on them, and types which implement `Identifiable`.
///
/// When a table is passed to `update`, every row in the table will be updated.
/// You can scope this down by calling [`filter`] which will
/// result in `UPDATE your_table SET ... WHERE args_to_filter`. Passing a type
/// which implements `Identifiable` is the same as passing
/// `SomeStruct::table().find(some_struct)`.
///
/// [`update`]: crate::update()
/// [`delete`]: crate::delete()
/// [`filter`]: crate::query_builder::UpdateStatement::filter()
pub trait IntoUpdateTarget: HasTable {
    /// Decomposes `self` into the table and where clause.
    fn into_update_target(self) -> UpdateTarget<Self::Table>;
}

impl<T, Tab> IntoUpdateTarget for T
where
    T: Identifiable<Table = Tab>,
    Tab: Table + FindDsl<T::Id>,
    Find<Tab, T::Id>: IntoUpdateTarget<Table = Tab>,
{
    fn into_update_target(self) -> UpdateTarget<Self::Table> {
        T::table().find(self.id()).into_update_target()
    }
}
