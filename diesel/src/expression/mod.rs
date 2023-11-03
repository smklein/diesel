//! AST types representing various typed SQL expressions.
//!
//! Almost all types implement either [`Expression`] or
//! [`AsExpression`].
//!
//! The most common expression to work with is a
//! [`Column`](crate::query_source::Column). There are various methods
//! that you can call on these, found in
//! [`expression_methods`](crate::expression_methods).
//!
//! You can also use numeric operators such as `+` on expressions of the
//! appropriate type.
//!
//! Any primitive which implements [`ToSql`](crate::serialize::ToSql) will
//! also implement [`AsExpression`], allowing it to be
//! used as an argument to any of the methods described here.
#[macro_use]
pub(crate) mod ops;
pub mod functions;

#[cfg(not(feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes"))]
pub(crate) mod array_comparison;
#[cfg(feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes")]
pub mod array_comparison;
pub(crate) mod assume_not_null;
pub(crate) mod bound;
mod coerce;
pub(crate) mod count;
#[cfg(not(feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes"))]
pub(crate) mod exists;
#[cfg(feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes")]
pub mod exists;
pub(crate) mod grouped;
pub(crate) mod helper_types;
mod not;
pub(crate) mod nullable;
#[macro_use]
pub(crate) mod operators;
pub(crate) mod select_by;
mod sql_literal;
pub(crate) mod subselect;

#[cfg(feature = "i-implement-a-third-party-backend-and-opt-into-breaking-changes")]
pub use self::operators::Concat;

// we allow unreachable_pub here
// as rustc otherwise shows false positives
// for every item in this module. We reexport
// everything from `crate::helper_types::`
#[allow(non_camel_case_types, unreachable_pub)]
pub(crate) mod dsl {
    use crate::dsl::SqlTypeOf;

    #[doc(inline)]
    pub use super::count::*;
    #[doc(inline)]
    pub use super::exists::exists;
    #[doc(inline)]
    pub use super::functions::aggregate_folding::*;
    #[doc(inline)]
    pub use super::functions::aggregate_ordering::*;
    #[doc(inline)]
    pub use super::functions::date_and_time::*;
    #[doc(inline)]
    pub use super::not::not;
    #[doc(inline)]
    pub use super::sql_literal::sql;

    #[cfg(feature = "postgres_backend")]
    pub use crate::pg::expression::dsl::*;

    /// The return type of [`count(expr)`](crate::dsl::count())
    pub type count<Expr> = super::count::count::HelperType<SqlTypeOf<Expr>, Expr>;

    /// The return type of [`count_star()`](crate::dsl::count_star())
    pub type count_star = super::count::CountStar;

    /// The return type of [`count_distinct()`](crate::dsl::count_distinct())
    pub type count_distinct<Expr> = super::count::CountDistinct<SqlTypeOf<Expr>, Expr>;

    /// The return type of [`date(expr)`](crate::dsl::date())
    pub type date<Expr> = super::functions::date_and_time::date::HelperType<Expr>;

    #[cfg(feature = "mysql_backend")]
    pub use crate::mysql::query_builder::DuplicatedKeys;
}

#[doc(inline)]
pub use self::sql_literal::{SqlLiteral, UncheckedBind};

use crate::backend::Backend;
use crate::dsl::{AsExprOf, AsSelect};
use crate::sql_types::{HasSqlType, SingleValue, SqlType};

/// Represents a typed fragment of SQL.
///
/// Apps should not need to implement this type directly, but it may be common
/// to use this in where clauses. Libraries should consider using
/// [`infix_operator!`](crate::infix_operator!) or
/// [`postfix_operator!`](crate::postfix_operator!) instead of
/// implementing this directly.
pub trait Expression {
    /// The type that this expression represents in SQL
    type SqlType: TypedExpressionType;
}

/// Marker trait for possible types of [`Expression::SqlType`]
///
pub trait TypedExpressionType {}

/// Possible types for []`Expression::SqlType`]
///
pub mod expression_types {
    use super::{QueryMetadata, TypedExpressionType};
    use crate::backend::Backend;
    use crate::sql_types::SingleValue;

    /// Query nodes with this expression type do not have a statically at compile
    /// time known expression type.
    ///
    /// An example for such a query node in diesel itself, is `sql_query` as
    /// we do not know which fields are returned from such a query at compile time.
    ///
    /// For loading values from queries returning a type of this expression, consider
    /// using [`#[derive(QueryableByName)]`](derive@crate::deserialize::QueryableByName)
    /// on the corresponding result type.
    ///
    #[derive(Clone, Copy, Debug)]
    pub struct Untyped;

    /// Query nodes witch cannot be part of a select clause.
    ///
    /// If you see an error message containing `FromSqlRow` and this type
    /// recheck that you have written a valid select clause
    #[derive(Debug, Clone, Copy)]
    pub struct NotSelectable;

    impl TypedExpressionType for Untyped {}
    impl TypedExpressionType for NotSelectable {}

    impl<ST> TypedExpressionType for ST where ST: SingleValue {}

    impl<DB: Backend> QueryMetadata<Untyped> for DB {
        fn row_metadata(_: &mut DB::MetadataLookup, row: &mut Vec<Option<DB::TypeMetadata>>) {
            row.push(None)
        }
    }
}

impl<T: Expression + ?Sized> Expression for Box<T> {
    type SqlType = T::SqlType;
}

impl<'a, T: Expression + ?Sized> Expression for &'a T {
    type SqlType = T::SqlType;
}

/// A helper to translate type level sql type information into
/// runtime type information for specific queries
///
/// If you do not implement a custom backend implementation
/// this trait is likely not relevant for you.
pub trait QueryMetadata<T>: Backend {
    /// The exact return value of this function is considered to be a
    /// backend specific implementation detail. You should not rely on those
    /// values if you not own the corresponding backend
    fn row_metadata(lookup: &mut Self::MetadataLookup, out: &mut Vec<Option<Self::TypeMetadata>>);
}

impl<T, DB> QueryMetadata<T> for DB
where
    DB: Backend + HasSqlType<T>,
    T: SingleValue,
{
    fn row_metadata(lookup: &mut Self::MetadataLookup, out: &mut Vec<Option<Self::TypeMetadata>>) {
        out.push(Some(<DB as HasSqlType<T>>::metadata(lookup)))
    }
}

/// Converts a type to its representation for use in Diesel's query builder.
///
/// This trait is used directly. Apps should typically use [`IntoSql`] instead.
///
/// Implementations of this trait will generally do one of 3 things:
///
/// - Return `self` for types which are already parts of Diesel's query builder
/// - Perform some implicit coercion (for example, allowing [`now`] to be used as
///   both [`Timestamp`] and [`Timestamptz`].
/// - Indicate that the type has data which will be sent separately from the
///   query. This is generally referred as a "bind parameter". Types which
///   implement [`ToSql`] will generally implement `AsExpression` this way.
///
///   [`IntoSql`]: crate::IntoSql
///   [`now`]: crate::dsl::now
///   [`Timestamp`]: crate::sql_types::Timestamp
///   [`Timestamptz`]: ../pg/types/sql_types/struct.Timestamptz.html
///   [`ToSql`]: crate::serialize::ToSql
///
///  This trait could be [derived](derive@AsExpression)
pub trait AsExpression<T>
where
    T: SqlType + TypedExpressionType,
{
    /// The expression being returned
    type Expression: Expression<SqlType = T>;

    /// Perform the conversion
    #[allow(clippy::wrong_self_convention)]
    // That's public API we cannot change it to appease clippy
    fn as_expression(self) -> Self::Expression;
}

#[doc(inline)]
pub use diesel_derives::AsExpression;

impl<T, ST> AsExpression<ST> for T
where
    T: Expression<SqlType = ST>,
    ST: SqlType + TypedExpressionType,
{
    type Expression = T;

    fn as_expression(self) -> T {
        self
    }
}

/// Converts a type to its representation for use in Diesel's query builder.
///
/// This trait only exists to make usage of `AsExpression` more ergonomic when
/// the `SqlType` cannot be inferred. It is generally used when you need to use
/// a Rust value as the left hand side of an expression, or when you want to
/// select a constant value.
///
/// # Example
///
/// ```rust
/// # include!("../doctest_setup.rs");
/// # use schema::users;
/// #
/// # fn main() {
/// use diesel::sql_types::Text;
/// #   let conn = &mut establish_connection();
/// let names = users::table
///     .select("The Amazing ".into_sql::<Text>().concat(users::name))
///     .load(conn);
/// let expected_names = vec![
///     "The Amazing Sean".to_string(),
///     "The Amazing Tess".to_string(),
/// ];
/// assert_eq!(Ok(expected_names), names);
/// # }
/// ```
pub trait IntoSql {
    /// Convert `self` to an expression for Diesel's query builder.
    ///
    /// There is no difference in behavior between `x.into_sql::<Y>()` and
    /// `AsExpression::<Y>::as_expression(x)`.
    fn into_sql<T>(self) -> AsExprOf<Self, T>
    where
        Self: AsExpression<T> + Sized,
        T: SqlType + TypedExpressionType,
    {
        self.as_expression()
    }

    /// Convert `&self` to an expression for Diesel's query builder.
    ///
    /// There is no difference in behavior between `x.as_sql::<Y>()` and
    /// `AsExpression::<Y>::as_expression(&x)`.
    fn as_sql<'a, T>(&'a self) -> AsExprOf<&'a Self, T>
    where
        &'a Self: AsExpression<T>,
        T: SqlType + TypedExpressionType,
    {
        <&'a Self as AsExpression<T>>::as_expression(self)
    }
}

impl<T> IntoSql for T {}

/// Indicates that all elements of an expression are valid given a from clause.
///
/// This is used to ensure that `users.filter(posts::id.eq(1))` fails to
/// compile. This constraint is only used in places where the nullability of a
/// SQL type doesn't matter (everything except `select` and `returning`). For
/// places where nullability is important, `SelectableExpression` is used
/// instead.
pub trait AppearsInQuery<QS: ?Sized>: Expression {}

// impl<T: ?Sized, QS> AppearsInQuery<QS> for Box<T>
// where
//     T: AppearsInQuery<QS>,
//     Box<T>: Expression,
// {
// }
// 
// impl<'a, T: ?Sized, QS> AppearsInQuery<QS> for &'a T
// where
//     T: AppearsInQuery<QS>,
//     &'a T: Expression,
// {
// }

/// Indicates that an expression can be selected from a source.
///
/// Columns will implement this for their table. Certain special types, like
/// `CountStar` and `Bound` will implement this for all sources. Most compound
/// expressions will implement this if each of their parts implement it.
///
/// Notably, columns will not implement this trait for the right side of a left
/// join. To select a column or expression using a column from the right side of
/// a left join, you must call `.nullable()` on it.
#[cfg_attr(
    feature = "nightly-error-messages",
    diagnostic::on_unimplemented(
        message = "Cannot select `{Self}` from `{QS}`",
        note = "`{Self}` is no valid selection for `{QS}`"
    )
)]
pub trait SelectableExpression<QS: ?Sized>: AppearsInQuery<QS> {}

// impl<T: ?Sized, QS> SelectableExpression<QS> for Box<T>
// where
//     T: SelectableExpression<QS>,
//     Box<T>: AppearsInQuery<QS>,
// {
// }

// impl<'a, T: ?Sized, QS> SelectableExpression<QS> for &'a T
// where
//     T: SelectableExpression<QS>,
//     &'a T: AppearsInQuery<QS>,
// {
// }

/// Trait indicating that a record can be selected and queried from the database.
///
/// Types which implement `Selectable` represent the select clause of a SQL query.
/// Use [`SelectableHelper::as_select()`] to construct the select clause. Once you
/// called `.select(YourType::as_select())` we enforce at the type system level that you
/// use the same type to load the query result into.
///
/// The constructed select clause can contain arbitrary expressions coming from different
/// tables. The corresponding [derive](derive@Selectable) provides a simple way to
/// construct a select clause matching fields to the corresponding table columns.
///
/// # Examples
///
/// If you just want to construct a select clause using an existing struct, you can use
/// `#[derive(Selectable)]`, See [`#[derive(Selectable)]`](derive@Selectable) for details.
///
///
/// ```rust
/// # include!("../doctest_setup.rs");
/// #
/// use schema::users;
///
/// #[derive(Queryable, PartialEq, Debug, Selectable)]
/// struct User {
///     id: i32,
///     name: String,
/// }
///
/// # fn main() {
/// #     run_test();
/// # }
/// #
/// # fn run_test() -> QueryResult<()> {
/// #     use schema::users::dsl::*;
/// #     let connection = &mut establish_connection();
/// let first_user = users.select(User::as_select()).first(connection)?;
/// let expected = User { id: 1, name: "Sean".into() };
/// assert_eq!(expected, first_user);
/// #     Ok(())
/// # }
/// ```
///
/// Alternatively, we can implement the trait for our struct manually.
///
/// ```rust
/// # include!("../doctest_setup.rs");
/// #
/// use schema::users;
/// use diesel::prelude::{Queryable, Selectable};
/// use diesel::backend::Backend;
///
/// #[derive(Queryable, PartialEq, Debug)]
/// struct User {
///     id: i32,
///     name: String,
/// }
///
/// impl<DB> Selectable<DB> for User
/// where
///     DB: Backend
/// {
///     type SelectExpression = (users::id, users::name);
///
///     fn construct_selection() -> Self::SelectExpression {
///         (users::id, users::name)
///     }
/// }
///
/// # fn main() {
/// #     run_test();
/// # }
/// #
/// # fn run_test() -> QueryResult<()> {
/// #     use schema::users::dsl::*;
/// #     let connection = &mut establish_connection();
/// let first_user = users.select(User::as_select()).first(connection)?;
/// let expected = User { id: 1, name: "Sean".into() };
/// assert_eq!(expected, first_user);
/// #     Ok(())
/// # }
/// ```
///
/// When selecting from joined tables, you can select from a
/// composition of types that implement `Selectable`. The simplest way
/// is to use a tuple of all the types you wish to select.
///
/// ```rust
/// # include!("../doctest_setup.rs");
/// use schema::{users, posts};
///
/// #[derive(Debug, PartialEq, Queryable, Selectable)]
/// struct User {
///     id: i32,
///     name: String,
/// }
///
/// #[derive(Debug, PartialEq, Queryable, Selectable)]
/// struct Post {
///     id: i32,
///     user_id: i32,
///     title: String,
/// }
///
/// # fn main() -> QueryResult<()> {
/// #     let connection = &mut establish_connection();
/// #
/// let (first_user, first_post) = users::table
///     .inner_join(posts::table)
///     .select(<(User, Post)>::as_select())
///     .first(connection)?;
///
/// let expected_user = User { id: 1, name: "Sean".into() };
/// assert_eq!(expected_user, first_user);
///
/// let expected_post = Post { id: 1, user_id: 1, title: "My first post".into() };
/// assert_eq!(expected_post, first_post);
/// #
/// #     Ok(())
/// # }
/// ```
///
/// If you want to load only a subset of fields, you can create types
/// with those fields and use them in the composition.
///
/// ```rust
/// # include!("../doctest_setup.rs");
/// use schema::{users, posts};
///
/// #[derive(Debug, PartialEq, Queryable, Selectable)]
/// struct User {
///     id: i32,
///     name: String,
/// }
///
/// #[derive(Debug, PartialEq, Queryable, Selectable)]
/// #[diesel(table_name = posts)]
/// struct PostTitle {
///     title: String,
/// }
///
/// # fn main() -> QueryResult<()> {
/// #     let connection = &mut establish_connection();
/// #
/// let (first_user, first_post_title) = users::table
///     .inner_join(posts::table)
///     .select(<(User, PostTitle)>::as_select())
///     .first(connection)?;
///
/// let expected_user = User { id: 1, name: "Sean".into() };
/// assert_eq!(expected_user, first_user);
///
/// let expected_post_title = PostTitle { title: "My first post".into() };
/// assert_eq!(expected_post_title, first_post_title);
/// #
/// #     Ok(())
/// # }
/// ```
///
/// You are not limited to using only tuples to build the composed
/// type. The [`Selectable`](derive@Selectable) derive macro allows
/// you to *embed* other types. This is useful when you want to
/// implement methods or traits on the composed type.
///
/// ```rust
/// # include!("../doctest_setup.rs");
/// use schema::{users, posts};
///
/// #[derive(Debug, PartialEq, Queryable, Selectable)]
/// struct User {
///     id: i32,
///     name: String,
/// }
///
/// #[derive(Debug, PartialEq, Queryable, Selectable)]
/// #[diesel(table_name = posts)]
/// struct PostTitle {
///     title: String,
/// }
///
/// #[derive(Debug, PartialEq, Queryable, Selectable)]
/// struct UserPost {
///     #[diesel(embed)]
///     user: User,
///     #[diesel(embed)]
///     post_title: PostTitle,
/// }
///
/// # fn main() -> QueryResult<()> {
/// #     let connection = &mut establish_connection();
/// #
/// let first_user_post = users::table
///     .inner_join(posts::table)
///     .select(UserPost::as_select())
///     .first(connection)?;
///
/// let expected_user_post = UserPost {
///     user: User {
///         id: 1,
///         name: "Sean".into(),
///     },
///     post_title: PostTitle {
///         title: "My first post".into(),
///     },
/// };
/// assert_eq!(expected_user_post, first_user_post);
/// #
/// #     Ok(())
/// # }
/// ```
///
/// If you want to avoid nesting types, you can use the
/// [`Selectable`](derive@Selectable) derive macro's
/// `select_expression` and `select_expression_type` attributes to
/// flatten the fields. If the `select_expression` is simple enough,
/// it is not necessary to specify `select_expression_type`
/// (most query fragments are supported for this).
///
/// ```rust
/// # include!("../doctest_setup.rs");
/// use schema::{users, posts};
///
/// #[derive(Debug, PartialEq, Queryable, Selectable)]
/// struct User {
///     id: i32,
///     name: String,
/// }
///
/// #[derive(Debug, PartialEq, Queryable, Selectable)]
/// #[diesel(table_name = posts)]
/// struct PostTitle {
///     title: String,
/// }
///
/// #[derive(Debug, PartialEq, Queryable, Selectable)]
/// struct UserPost {
///     #[diesel(select_expression = users::columns::id)]
///     #[diesel(select_expression_type = users::columns::id)]
///     id: i32,
///     #[diesel(select_expression = users::columns::name)]
///     name: String,
///     #[diesel(select_expression = posts::columns::title)]
///     title: String,
/// }
///
/// # fn main() -> QueryResult<()> {
/// #     let connection = &mut establish_connection();
/// #
/// let first_user_post = users::table
///     .inner_join(posts::table)
///     .select(UserPost::as_select())
///     .first(connection)?;
///
/// let expected_user_post = UserPost {
///     id: 1,
///     name: "Sean".into(),
///     title: "My first post".into(),
/// };
/// assert_eq!(expected_user_post, first_user_post);
/// #
/// #     Ok(())
/// # }
/// ```
///
pub trait Selectable<DB: Backend> {
    /// The expression you'd like to select.
    ///
    /// This is typically a tuple of corresponding to the table columns of your struct's fields.
    type SelectExpression: Expression;

    /// Construct an instance of the expression
    fn construct_selection() -> Self::SelectExpression;
}

#[doc(inline)]
pub use diesel_derives::Selectable;

/// This helper trait provides several methods for
/// constructing a select or returning clause based on a
/// [`Selectable`] implementation.
pub trait SelectableHelper<DB: Backend>: Selectable<DB> + Sized {
    /// Construct a select clause based on a [`Selectable`] implementation.
    ///
    /// The returned select clause enforces that you use the same type
    /// for constructing the select clause and for loading the query result into.
    fn as_select() -> AsSelect<Self, DB>;

    /// An alias for `as_select` that can be used with returning clauses
    fn as_returning() -> AsSelect<Self, DB> {
        Self::as_select()
    }
}

impl<T, DB> SelectableHelper<DB> for T
where
    T: Selectable<DB>,
    DB: Backend,
{
    fn as_select() -> AsSelect<Self, DB> {
        select_by::SelectBy::new()
    }
}

/// TODO: Remove me!
pub trait ValidGrouping<GroupByClause> {
    /// TODO: Remove me!
    type IsAggregate;
}
#[doc(inline)]
pub use diesel_derives::ValidGrouping;

use crate::query_builder::{QueryFragment, QueryId};

/// Helper trait used when boxing expressions.
///
/// In Rust you cannot create a trait object with more than one trait.
/// This type has all of the additional traits you would want when using
/// `Box<Expression>` as a single trait object.
///
/// By default `BoxableExpression` is not usable in queries that have a custom
/// group by clause. Setting the generic parameters `GB` and `IsAggregate` allows
/// to configure the expression to be used with a specific group by clause.
///
/// This is typically used as the return type of a function.
/// For cases where you want to dynamically construct a query,
/// [boxing the query] is usually more ergonomic.
///
/// [boxing the query]: crate::query_dsl::QueryDsl::into_boxed()
///
/// # Examples
///
/// ## Usage without group by clause
///
/// ```rust
/// # include!("../doctest_setup.rs");
/// # use schema::users;
/// use diesel::sql_types::Bool;
///
/// # fn main() {
/// #     run_test().unwrap();
/// # }
/// #
/// # fn run_test() -> QueryResult<()> {
/// #     let conn = &mut establish_connection();
/// enum Search {
///     Id(i32),
///     Name(String),
/// }
///
/// # /*
/// type DB = diesel::sqlite::Sqlite;
/// # */
///
/// fn find_user(search: Search) -> Box<dyn BoxableExpression<users::table, DB, SqlType = Bool>> {
///     match search {
///         Search::Id(id) => Box::new(users::id.eq(id)),
///         Search::Name(name) => Box::new(users::name.eq(name)),
///     }
/// }
///
/// let user_one = users::table
///     .filter(find_user(Search::Id(1)))
///     .first(conn)?;
/// assert_eq!((1, String::from("Sean")), user_one);
///
/// let tess = users::table
///     .filter(find_user(Search::Name("Tess".into())))
///     .first(conn)?;
/// assert_eq!((2, String::from("Tess")), tess);
/// #     Ok(())
/// # }
/// ```
///
/// ## Allow usage with group by clause
///
/// ```rust
/// # include!("../doctest_setup.rs");
///
/// # use schema::users;
/// use diesel::sql_types::Text;
/// use diesel::dsl;
///
/// # fn main() {
/// #     run_test().unwrap();
/// # }
/// #
/// # fn run_test() -> QueryResult<()> {
/// #     let conn = &mut establish_connection();
/// enum NameOrConst {
///     Name,
///     Const(String),
/// }
///
/// # /*
/// type DB = diesel::sqlite::Sqlite;
/// # */
///
/// fn selection<GB>(
///     selection: NameOrConst
/// ) -> Box<
///     dyn BoxableExpression<
///         users::table,
///         DB,
///         GB,
///         SqlType = Text
///     >
/// >
/// where
///     users::name: BoxableExpression<
///             users::table,
///             DB,
///             GB,
///             SqlType = Text
///         >
/// {
///     match selection {
///         NameOrConst::Name => Box::new(users::name),
///         NameOrConst::Const(name) => Box::new(name.into_sql::<Text>()),
///     }
/// }
///
/// let user_one = users::table
///     .select(selection(NameOrConst::Name))
///     .first::<String>(conn)?;
/// assert_eq!(String::from("Sean"), user_one);
///
/// let with_name = users::table
///     .group_by(users::name)
///     .select(selection(NameOrConst::Const("Jane Doe".into())))
///     .first::<String>(conn)?;
/// assert_eq!(String::from("Jane Doe"), with_name);
/// #     Ok(())
/// # }
/// ```
///
/// ## More advanced query source
///
/// This example is a bit contrived, but in general, if you want to for example filter based on
/// different criteria on a joined table, you can use `InnerJoinQuerySource` and
/// `LeftJoinQuerySource` in the QS parameter of `BoxableExpression`.
///
/// ```rust
/// # include!("../doctest_setup.rs");
/// # use schema::{users, posts};
/// use diesel::sql_types::Bool;
/// use diesel::dsl::InnerJoinQuerySource;
///
/// # fn main() {
/// #     run_test().unwrap();
/// # }
/// #
/// # fn run_test() -> QueryResult<()> {
/// #     let conn = &mut establish_connection();
/// enum UserPostFilter {
///     User(i32),
///     Post(i32),
/// }
///
/// # /*
/// type DB = diesel::sqlite::Sqlite;
/// # */
///
/// fn filter_user_posts(
///     filter: UserPostFilter,
/// ) -> Box<dyn BoxableExpression<InnerJoinQuerySource<users::table, posts::table>, DB, SqlType = Bool>>
/// {
///     match filter {
///         UserPostFilter::User(user_id) => Box::new(users::id.eq(user_id)),
///         UserPostFilter::Post(post_id) => Box::new(posts::id.eq(post_id)),
///     }
/// }
///
/// let post_by_user_one = users::table
///     .inner_join(posts::table)
///     .filter(filter_user_posts(UserPostFilter::User(2)))
///     .select((posts::title, users::name))
///     .first::<(String, String)>(conn)?;
///
/// assert_eq!(
///     ("My first post too".to_string(), "Tess".to_string()),
///     post_by_user_one
/// );
/// #     Ok(())
/// # }
/// ```
pub trait BoxableExpression<QS, DB, GB = ()>
where
    DB: Backend,
    Self: Expression,
    Self: SelectableExpression<QS>,
    Self: QueryFragment<DB>,
    Self: Send,
{
}

impl<QS, T, DB, GB> BoxableExpression<QS, DB, GB> for T
where
    DB: Backend,
    T: Expression,
    T: SelectableExpression<QS>,
    T: QueryFragment<DB>,
    T: Send,
{
}

impl<'a, QS, ST, DB, GB> QueryId
    for dyn BoxableExpression<QS, DB, GB, SqlType = ST> + 'a
{
    type QueryId = ();

    const HAS_STATIC_QUERY_ID: bool = false;
}

/// Converts a tuple of values into a tuple of Diesel expressions.
///
/// This trait is similar to [`AsExpression`], but it operates on tuples.
/// The expressions must all be of the same SQL type.
///
pub trait AsExpressionList<ST> {
    /// The final output expression
    type Expression;

    /// Perform the conversion
    // That's public API, we cannot change
    // that to appease clippy
    #[allow(clippy::wrong_self_convention)]
    fn as_expression_list(self) -> Self::Expression;
}
