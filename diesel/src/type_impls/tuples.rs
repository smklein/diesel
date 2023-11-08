use crate::associations::BelongsTo;
use crate::deserialize::{
    self, FromSqlRow, FromStaticSqlRow, Queryable, SqlTypeOrSelectable, StaticallySizedRow,
};
use crate::expression::{
    AppearsInQuery, AsExpression, AsExpressionList, Expression,
    QueryMetadata, Selectable, SelectableExpression,
    TypedExpressionType,
};
use crate::insertable::{CanInsertInSingleQuery, InsertValues, Insertable, InsertableOptionHelper};
use crate::query_builder::*;
use crate::query_dsl::load_dsl::CompatibleType;
use crate::query_source::*;
use crate::result::QueryResult;
use crate::row::*;
use crate::sql_types::{HasSqlType, IntoNullable, Nullable, OneIsNullable, SqlType, TypeMetadata};
use crate::util::{TupleAppend, TupleSize};

impl<T> TupleSize for T
where
    T: crate::sql_types::SingleValue,
{
    const SIZE: usize = 1;
}

macro_rules! tuple_impls {
    ($(
        $Tuple:tt {
            $(($idx:tt) -> $T:ident, $ST:ident, $TT:ident,)+
        }
    )+) => {
        $(
            impl<$($T),+> HasSqlType<($($T,)+)> for DB where
                $(DB: HasSqlType<$T>),+,
            {
                fn metadata(_: &mut <DB as TypeMetadata>::MetadataLookup) -> <DB as TypeMetadata>::TypeMetadata {
                    unreachable!("Tuples should never implement `ToSql` directly");
                }
            }

            impl_from_sql_row!(($($T,)+), ($($ST,)+));


            impl<$($T: Expression),+> Expression for ($($T,)+)
            where ($($T::SqlType, )*): TypedExpressionType
            {
                type SqlType = ($(<$T as Expression>::SqlType,)+);
            }

            impl<$($T: TypedExpressionType,)*> TypedExpressionType for ($($T,)*) {}
            impl<$($T: SqlType + TypedExpressionType,)*> TypedExpressionType for Nullable<($($T,)*)>
            where ($($T,)*): SqlType
            {
            }

            impl<$($T: SqlType,)*> IntoNullable for ($($T,)*)
                where Self: SqlType,
            {
                type Nullable = Nullable<($($T,)*)>;
            }

            impl<$($T,)+> Selectable for ($($T,)+)
            where
                $($T: Selectable),+,
            {
                type SelectExpression = ($($T::SelectExpression,)+);

                fn construct_selection() -> Self::SelectExpression {
                    ($($T::construct_selection(),)+)
                }
            }

            impl<$($T: QueryFragment),+> QueryFragment for ($($T,)+) {
                #[allow(unused_assignments)]
                fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b, DB>) -> QueryResult<()>
                {
                    let mut needs_comma = false;
                    $(
                        if !self.$idx.is_noop(out.backend())? {
                            if needs_comma {
                                out.push_sql(", ");
                            }
                            self.$idx.walk_ast(out.reborrow())?;
                            needs_comma = true;
                        }
                    )+
                    Ok(())
                }
            }

            impl<$($T,)+ > ColumnList for ($($T,)+)
            where
                $($T: ColumnList,)+
            {
                fn walk_ast(&self, mut out: AstPass<'_, '_, DB>) -> QueryResult<()> {
                    $(
                        if $idx != 0 {
                            out.push_sql(", ");
                        }
                        self.$idx.walk_ast(out.reborrow())?;
                    )+
                    Ok(())
                }
            }

            impl<$($T: QueryId),+> QueryId for ($($T,)+) {
                type QueryId = ($($T::QueryId,)+);

                const HAS_STATIC_QUERY_ID: bool = $($T::HAS_STATIC_QUERY_ID &&)+ true;
            }

            impl<$($T,)+ Tab> UndecoratedInsertRecord<Tab> for ($($T,)+)
            where
                $($T: UndecoratedInsertRecord<Tab>,)+
            {
            }

            impl<$($T,)+ > CanInsertInSingleQuery for ($($T,)+)
            where
                $($T: CanInsertInSingleQuery,)+
            {
                fn rows_to_insert(&self) -> Option<usize> {
                    $(debug_assert_eq!(self.$idx.rows_to_insert(), Some(1));)+
                    Some(1)
                }
            }

            impl<$($T,)+ $($ST,)+ Tab> Insertable<Tab> for ($($T,)+)
            where
                $($T: Insertable<Tab, Values = ValuesClause<$ST, Tab>>,)+
            {
                type Values = ValuesClause<($($ST,)+), Tab>;

                fn values(self) -> Self::Values {
                    ValuesClause::new(($(self.$idx.values().values,)+))
                }
            }

            impl<'a, $($T,)+ Tab> Insertable<Tab> for &'a ($($T,)+)
            where
                ($(&'a $T,)+): Insertable<Tab>,
            {
                type Values = <($(&'a $T,)+) as Insertable<Tab>>::Values;

                fn values(self) -> Self::Values {
                    ($(&self.$idx,)+).values()
                }
            }

            #[allow(unused_assignments)]
            impl<$($T,)+ Tab> InsertValues<Tab> for ($($T,)+)
            where
                Tab: Table,
                $($T: InsertValues<Tab>,)+
            {
                fn column_names(&self, mut out: AstPass<'_, '_, DB>) -> QueryResult<()> {
                    let mut needs_comma = false;
                    $(
                        let noop_element = self.$idx.is_noop(out.backend())?;
                        if !noop_element {
                            if needs_comma {
                                out.push_sql(", ");
                            }
                            self.$idx.column_names(out.reborrow())?;
                            needs_comma = true;
                        }
                    )+
                    Ok(())
                } }

            impl<__T, $($ST,)* Tab> Insertable<Tab> for InsertableOptionHelper<__T, ($($ST,)*)>
            where
                __T: Insertable<Tab>,
                __T::Values: Default,
            {
                type Values = __T::Values;

                fn values(self) -> Self::Values {
                    self.0.map(|v| v.values()).unwrap_or_default()
                }
            }

            impl<$($T,)+ QS> SelectableExpression<QS> for ($($T,)+) where
                $($T: SelectableExpression<QS>,)+
                ($($T,)+): AppearsInQuery<QS>,
            {
            }

            impl<$($T,)+ QS> AppearsInQuery<QS> for ($($T,)+) where
                $($T: AppearsInQuery<QS>,)+
                ($($T,)+): Expression,
            {
            }

            impl<Target, $($T,)+> AsChangeset for ($($T,)+) where
                $($T: AsChangeset<Target=Target>,)+
                Target: QuerySource,
            {
                type Target = Target;
                type Changeset = ($($T::Changeset,)+);

                fn as_changeset(self) -> Self::Changeset {
                    ($(self.$idx.as_changeset(),)+)
                }
            }

            impl<$($T,)+ Parent> BelongsTo<Parent> for ($($T,)+) where
                T0: BelongsTo<Parent>,
            {
                type ForeignKey = T0::ForeignKey;
                type ForeignKeyColumn = T0::ForeignKeyColumn;

                fn foreign_key(&self) -> Option<&Self::ForeignKey> {
                    self.0.foreign_key()
                }

                fn foreign_key_column() -> Self::ForeignKeyColumn {
                    T0::foreign_key_column()
                }
            }

            impl<$($T,)+ Next> TupleAppend<Next> for ($($T,)+) {
                type Output = ($($T,)+ Next);

                #[allow(non_snake_case)]
                fn tuple_append(self, next: Next) -> Self::Output {
                    let ($($T,)+) = self;
                    ($($T,)+ next)
                }
            }

            impl<$($T,)+ ST> AsExpressionList<ST> for ($($T,)+) where
                $($T: AsExpression<ST>,)+
                ST: SqlType + TypedExpressionType,
            {
                type Expression = ($($T::Expression,)+);

                fn as_expression_list(self) -> Self::Expression {
                    ($(self.$idx.as_expression(),)+)
                }
            }

            impl_sql_type!($($T,)*);

            impl<$($T,)*  $($ST,)*> Queryable<($($ST,)*)> for ($($T,)*)
            where Self: FromStaticSqlRow<($($ST,)*)>,
            {
                type Row = Self;

                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }

            impl<__T, $($ST,)*> FromStaticSqlRow<Nullable<($($ST,)*)>> for Option<__T> where
                ($($ST,)*): SqlType,
                __T: FromSqlRow<($($ST,)*)>,
            {

                #[allow(non_snake_case, unused_variables, unused_mut)]
                fn build_from_row<'a>(row: &impl Row<'a, DB>)
                                      -> deserialize::Result<Self>
                {
                    match <__T as FromSqlRow<($($ST,)*)>>::build_from_row(row) {
                        Ok(v) => Ok(Some(v)),
                        Err(e) if e.is::<crate::result::UnexpectedNullError>() => Ok(None),
                        Err(e) => Err(e)
                    }
                }
            }

            impl<__T, $($ST,)*> Queryable<Nullable<($($ST,)*)>> for Option<__T>
            where Self: FromStaticSqlRow<Nullable<($($ST,)*)>>,
                  ($($ST,)*): SqlType,
            {
                type Row = Self;

                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }

            impl<$($T,)*> TupleSize for ($($T,)*)
                where $($T: TupleSize,)*
            {
                const SIZE: usize = $($T::SIZE +)* 0;
            }

            impl<$($T,)*> TupleSize for Nullable<($($T,)*)>
            where $($T: TupleSize,)*
                  ($($T,)*): SqlType,
            {
                const SIZE: usize = $($T::SIZE +)* 0;
            }

            impl<$($T,)*> QueryMetadata<($($T,)*)> for DB
            where $(DB: QueryMetadata<$T>,)*
            {
                fn row_metadata(lookup: &mut Self::MetadataLookup, row: &mut Vec<Option<<DB as TypeMetadata>::TypeMetadata>>) {
                    $(
                        <DB as QueryMetadata<$T>>::row_metadata(lookup, row);
                    )*
                }
            }

            impl<$($T,)*> QueryMetadata<Nullable<($($T,)*)>> for DB
            where $(DB: QueryMetadata<$T>,)*
            {
                fn row_metadata(lookup: &mut Self::MetadataLookup, row: &mut Vec<Option<<DB as TypeMetadata>::TypeMetadata>>) {
                    $(
                        <DB as QueryMetadata<$T>>::row_metadata(lookup, row);
                    )*
                }
            }

            impl<$($T,)*> deserialize::QueryableByName for ($($T,)*)
            where $($T: deserialize::QueryableByName,)*
            {
                fn build<'a>(row: &impl NamedRow<'a, DB>) -> deserialize::Result<Self> {
                    Ok(($(
                        <$T as deserialize::QueryableByName>::build(row)?,
                    )*))
                }
            }

            impl<__T, $($ST,)*> CompatibleType<__T> for ($($ST,)*)
            where
                __T: FromSqlRow<($($ST,)*)>,
            {
                type SqlType = Self;
            }

            impl<__T, $($ST,)*> CompatibleType<Option<__T>> for Nullable<($($ST,)*)>
            where
                ($($ST,)*): CompatibleType<__T>
            {
                type SqlType = Nullable<<($($ST,)*) as CompatibleType<__T>>::SqlType>;
            }

            impl<$($ST,)*> SqlTypeOrSelectable for ($($ST,)*)
            where $($ST: SqlTypeOrSelectable,)*
            {}

            impl<$($ST,)*> SqlTypeOrSelectable for Nullable<($($ST,)*)>
            where ($($ST,)*): SqlTypeOrSelectable
            {}
        )+
    }
}

macro_rules! impl_from_sql_row {
    (($T1: ident,), ($ST1: ident,)) => {
        impl<$T1, $ST1> crate::deserialize::FromStaticSqlRow<($ST1,)> for ($T1,) where
            $ST1: CompatibleType<$T1>,
            $T1: FromSqlRow<<$ST1 as CompatibleType<$T1>>::SqlType>,
        {

            #[allow(non_snake_case, unused_variables, unused_mut)]
            fn build_from_row<'a>(row: &impl Row<'a, DB>)
                                                       -> deserialize::Result<Self>
            {
                Ok(($T1::build_from_row(row)?,))
            }
        }
    };
    (($T1: ident, $($T: ident,)*), ($ST1: ident, $($ST: ident,)*)) => {
        impl<$T1, $($T,)* $($ST,)*> FromSqlRow<($($ST,)* crate::sql_types::Untyped)> for ($($T,)* $T1)
        where $T1: FromSqlRow<crate::sql_types::Untyped>,
            $(
                $T: FromSqlRow<$ST> + StaticallySizedRow<$ST>,
        )*
        {
            #[allow(non_snake_case, unused_variables, unused_mut)]
            fn build_from_row<'a>(full_row: &impl Row<'a, DB>)
                -> deserialize::Result<Self>
            {
                let field_count = full_row.field_count();

                let mut static_field_count = 0;
                $(
                    let row = full_row.partial_row(static_field_count..static_field_count + $T::FIELD_COUNT);
                    static_field_count += $T::FIELD_COUNT;
                    let $T = $T::build_from_row(&row)?;
                )*

                let row = full_row.partial_row(static_field_count..field_count);

                Ok(($($T,)* $T1::build_from_row(&row)?,))
            }
        }

        impl<$T1, $ST1, $($T,)* $($ST,)*> FromStaticSqlRow<($($ST,)* $ST1,)> for ($($T,)* $T1,) where
            $ST1: CompatibleType<$T1>,
            $T1: FromSqlRow<<$ST1 as CompatibleType<$T1>>::SqlType>,
            $(
                $ST: CompatibleType<$T>,
                $T: FromSqlRow<<$ST as CompatibleType<$T>>::SqlType> + StaticallySizedRow<<$ST as CompatibleType<$T>>::SqlType>,
            )*

        {

            #[allow(non_snake_case, unused_variables, unused_mut)]
            fn build_from_row<'a>(full_row: &impl Row<'a, DB>)
                -> deserialize::Result<Self>
            {
                let field_count = full_row.field_count();

                let mut static_field_count = 0;
                $(
                    let row = full_row.partial_row(static_field_count..static_field_count + $T::FIELD_COUNT);
                    static_field_count += $T::FIELD_COUNT;
                    let $T = <$T as FromSqlRow<<$ST as CompatibleType<$T>>::SqlType>>::build_from_row(&row)?;
                )*

                let row = full_row.partial_row(static_field_count..field_count);

                Ok(($($T,)* $T1::build_from_row(&row)?,))
            }
        }
    }
}

macro_rules! impl_sql_type {
    (
        @build
        start_ts = [$($ST: ident,)*],
        ts = [$T1: ident,],
        bounds = [$($bounds: tt)*],
        is_null = [$($is_null: tt)*],
    )=> {
        impl<$($ST,)*> SqlType for ($($ST,)*)
        where
            $($ST: SqlType,)*
            $($bounds)*
            $T1::IsNull: OneIsNullable<$($is_null)*>,
        {
            type IsNull = <$T1::IsNull as OneIsNullable<$($is_null)*>>::Out;
        }

    };
    (
        @build
        start_ts = [$($ST: ident,)*],
        ts = [$T1: ident, $($T: ident,)+],
        bounds = [$($bounds: tt)*],
        is_null = [$($is_null: tt)*],
    )=> {
        impl_sql_type!{
            @build
            start_ts = [$($ST,)*],
            ts = [$($T,)*],
            bounds = [$($bounds)* $T1::IsNull: OneIsNullable<$($is_null)*>,],
            is_null = [<$T1::IsNull as OneIsNullable<$($is_null)*>>::Out],
        }
    };
    ($T1: ident, $($T: ident,)+) => {
        impl_sql_type!{
            @build
            start_ts = [$T1, $($T,)*],
            ts = [$($T,)*],
            bounds = [],
            is_null = [$T1::IsNull],
        }
    };
    ($T1: ident,) => {
        impl<$T1> SqlType for ($T1,)
        where $T1: SqlType,
        {
            type IsNull = $T1::IsNull;
        }
    }
}

diesel_derives::__diesel_for_each_tuple!(tuple_impls);
