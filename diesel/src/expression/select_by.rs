use crate::backend::{Backend, DieselReserveSpecialization};
use crate::dsl::SqlTypeOf;
use crate::expression::{
    AppearsInQuery, Expression, QueryMetadata, Selectable, SelectableExpression,
    TypedExpressionType,
};
use crate::query_builder::*;
use crate::result::QueryResult;

#[derive(Debug)]
pub struct SelectBy<T: Selectable> {
    selection: T::SelectExpression,
    p: std::marker::PhantomData<T>,
}

impl<T> Clone for SelectBy<T>
where
    T: Selectable,
{
    fn clone(&self) -> Self {
        Self {
            selection: T::construct_selection(),
            p: std::marker::PhantomData,
        }
    }
}

impl<T> Copy for SelectBy<T>
where
    T: Selectable,
    T::SelectExpression: Copy,
{
}

impl<T, E> QueryId for SelectBy<T>
where
    T: Selectable<SelectExpression = E>,
    E: QueryId + Expression,
{
    type QueryId = E::QueryId;

    const HAS_STATIC_QUERY_ID: bool = E::HAS_STATIC_QUERY_ID;
}

impl<T> SelectBy<T>
where
    T: Selectable,
{
    pub(crate) fn new() -> Self {
        Self {
            selection: T::construct_selection(),
            p: std::marker::PhantomData,
        }
    }
}

impl<T, E> Expression for SelectBy<T>
where
    T: Selectable<SelectExpression = E>,
    E: QueryId + Expression,
{
    type SqlType = SelectBy<T>;
}

impl<T> TypedExpressionType for SelectBy<T>
where
    T: Selectable,
{
}

impl<T> QueryMetadata<SelectBy<T>> for DB
where
    T: Selectable,
    DB: QueryMetadata<SqlTypeOf<T::SelectExpression>>,
{
    fn row_metadata(lookup: &mut Self::MetadataLookup, out: &mut Vec<Option<Self::TypeMetadata>>) {
        <DB as QueryMetadata<SqlTypeOf<<T as Selectable>::SelectExpression>>>::row_metadata(
            lookup, out,
        )
    }
}

impl<T> QueryFragment for SelectBy<T>
where
    T: Selectable,
    T::SelectExpression: QueryFragment,
    DB: Backend + DieselReserveSpecialization,
{
    fn walk_ast<'b>(&'b self, out: AstPass<'_, 'b>) -> QueryResult<()> {
        self.selection.walk_ast(out)
    }
}

impl<T, QS> SelectableExpression<QS> for SelectBy<T>
where
    T: Selectable,
    T::SelectExpression: SelectableExpression<QS>,
    Self: AppearsInQuery<QS>,
{
}

impl<T, QS> AppearsInQuery<QS> for SelectBy<T>
where
    T: Selectable,
    T::SelectExpression: AppearsInQuery<QS>,
    Self: Expression,
{
}
