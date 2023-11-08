use self::private::LoadIter;
use super::RunQueryDsl;
use crate::backend::Backend;
use crate::connection::{Connection, DefaultLoadingMode, LoadConnection};
use crate::deserialize::FromSqlRow;
use crate::expression::QueryMetadata;
use crate::query_builder::{AsQuery, DB, QueryFragment, QueryId};
use crate::result::QueryResult;

#[cfg(feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes")]
pub use self::private::CompatibleType;

#[cfg(not(feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes"))]
pub(crate) use self::private::CompatibleType;

/// The `load` method
///
/// This trait should not be relied on directly by most apps. Its behavior is
/// provided by [`RunQueryDsl`]. However, you may need a where clause on this trait
/// to call `load` from generic code.
///
/// [`RunQueryDsl`]: crate::RunQueryDsl
pub trait LoadQuery<'query, Conn, U, B = DefaultLoadingMode>: RunQueryDsl<Conn> {
    /// Return type of `LoadQuery::internal_load`
    type RowIter<'conn>: Iterator<Item = QueryResult<U>>
    where
        Conn: 'conn;

    /// Load this query
    #[diesel_derives::__diesel_public_if(
        feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes"
    )]
    fn internal_load(self, conn: &mut Conn) -> QueryResult<Self::RowIter<'_>>;
}

#[doc(hidden)]
#[cfg(all(feature = "with-deprecated", not(feature = "without-deprecated")))]
#[deprecated(note = "Use `LoadQuery::Iter` directly")]
pub type LoadRet<'conn, 'query, Q, C, U, B = DefaultLoadingMode> =
    <Q as LoadQuery<'query, C, U, B>>::RowIter<'conn>;

impl<'query, Conn, T, U, B> LoadQuery<'query, Conn, U, B> for T
where
    Conn: Connection<Backend=DB> + LoadConnection<B>,
    T: AsQuery + RunQueryDsl<Conn>,
    T::Query: QueryFragment + QueryId + 'query,
    T::SqlType: CompatibleType<U>,
    DB: QueryMetadata<T::SqlType> + 'static,
    U: FromSqlRow<<T::SqlType as CompatibleType<U>>::SqlType> + 'static,
    <T::SqlType as CompatibleType<U>>::SqlType: 'static,
{
    type RowIter<'conn> = LoadIter<
        U,
        <Conn as LoadConnection<B>>::Cursor<'conn, 'query>,
        <T::SqlType as CompatibleType<U>>::SqlType,
    > where Conn: 'conn;

    fn internal_load(self, conn: &mut Conn) -> QueryResult<Self::RowIter<'_>> {
        Ok(LoadIter {
            cursor: conn.load(self.as_query())?,
            _marker: Default::default(),
        })
    }
}

/// The `execute` method
///
/// This trait should not be relied on directly by most apps. Its behavior is
/// provided by [`RunQueryDsl`]. However, you may need a where clause on this trait
/// to call `execute` from generic code.
///
/// [`RunQueryDsl`]: crate::RunQueryDsl
pub trait ExecuteDsl<Conn: Connection<Backend = DB>>:
    Sized
{
    /// Execute this command
    fn execute(query: Self, conn: &mut Conn) -> QueryResult<usize>;
}

use crate::result::Error;

impl<Conn, T> ExecuteDsl<Conn> for T
where
    Conn: Connection<Backend = DB>,
    T: QueryFragment + QueryId,
{
    fn execute(query: T, conn: &mut Conn) -> Result<usize, Error> {
        conn.execute_returning_count(&query)
    }
}

// These types and traits are not part of the public API.
//
// * CompatibleType as we consider this as "sealed" trait. It shouldn't
// be implemented by a third party
// * LoadIter as it's an implementation detail
mod private {
    use crate::deserialize::FromSqlRow;
    use crate::expression::select_by::SelectBy;
    use crate::expression::{Expression, TypedExpressionType};
    use crate::query_builder::DB;
    use crate::sql_types::{SqlType, Untyped};
    use crate::{QueryResult, Selectable};

    #[allow(missing_debug_implementations)]
    pub struct LoadIter<U, C, ST> {
        pub(super) cursor: C,
        pub(super) _marker: std::marker::PhantomData<(ST, U)>,
    }

    impl<'a, C, U, ST, R> LoadIter<U, C, ST>
    where
        C: Iterator<Item = QueryResult<R>>,
        R: crate::row::Row<'a, DB>,
        U: FromSqlRow<ST>,
    {
        pub(super) fn map_row(row: Option<QueryResult<R>>) -> Option<QueryResult<U>> {
            match row? {
                Ok(row) => Some(
                    U::build_from_row(&row).map_err(crate::result::Error::DeserializationError),
                ),
                Err(e) => Some(Err(e)),
            }
        }
    }

    impl<'a, C, U, ST, R> Iterator for LoadIter<U, C, ST>
    where
        C: Iterator<Item = QueryResult<R>>,
        R: crate::row::Row<'a, DB>,
        U: FromSqlRow<ST>,
    {
        type Item = QueryResult<U>;

        fn next(&mut self) -> Option<Self::Item> {
            Self::map_row(self.cursor.next())
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            self.cursor.size_hint()
        }

        fn count(self) -> usize
        where
            Self: Sized,
        {
            self.cursor.count()
        }

        fn last(self) -> Option<Self::Item>
        where
            Self: Sized,
        {
            Self::map_row(self.cursor.last())
        }

        fn nth(&mut self, n: usize) -> Option<Self::Item> {
            Self::map_row(self.cursor.nth(n))
        }
    }

    impl<'a, C, U, ST, R> ExactSizeIterator for LoadIter<U, C, ST>
    where
        C: ExactSizeIterator + Iterator<Item = QueryResult<R>>,
        R: crate::row::Row<'a, DB>,
        U: FromSqlRow<ST>,
    {
        fn len(&self) -> usize {
            self.cursor.len()
        }
    }

    #[cfg_attr(
        doc_cfg,
        doc(cfg(feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes"))
    )]
    #[cfg_attr(
        feature = "nightly-error-messages",
        diagnostic::on_unimplemented(
            note = "This is a mismatch between what your query returns and what your type expects the query to return\n\
                Consider using `#[derive(Selectable)]` + `#[diesel(check_for_backend({DB}))]` on your struct `{U}` and \n\
                in your query `.select({U}::as_select())` to get a better error message",
        )
    )]
    pub trait CompatibleType<U> {
        type SqlType;
    }

    impl<ST, U> CompatibleType<U> for ST
    where
        ST: SqlType + crate::sql_types::SingleValue,
        U: FromSqlRow<ST>,
    {
        type SqlType = ST;
    }

    impl<U> CompatibleType<U> for Untyped
    where
        U: FromSqlRow<Untyped>,
    {
        type SqlType = Untyped;
    }

    impl<U, E, ST> CompatibleType<U> for SelectBy<U>
    where
        ST: SqlType + TypedExpressionType,
        U: Selectable<SelectExpression = E>,
        E: Expression<SqlType = ST>,
        U: FromSqlRow<ST>,
    {
        type SqlType = ST;
    }
}
