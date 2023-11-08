use crate::query_builder::*;
use crate::result::QueryResult;
use std::marker::PhantomData;

#[doc(hidden)] // used by the table macro
pub trait StaticQueryFragment {
    type Component: 'static;
    const STATIC_COMPONENT: &'static Self::Component;
}

#[derive(Debug, Copy, Clone)]
#[doc(hidden)] // used by the table macro
pub struct StaticQueryFragmentInstance<T>(PhantomData<T>);

impl<T> StaticQueryFragmentInstance<T> {
    #[doc(hidden)] // used by the table macro
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<T> QueryFragment for StaticQueryFragmentInstance<T>
where
    T: StaticQueryFragment,
    T::Component: QueryFragment,
{
    fn walk_ast<'b>(&'b self, pass: AstPass<'_, 'b>) -> QueryResult<()> {
        T::STATIC_COMPONENT.walk_ast(pass)
    }
}

#[derive(Debug, Copy, Clone)]
#[doc(hidden)] // used by the table macro
pub struct Identifier<'a>(pub &'a str);

impl<'a> QueryFragment for Identifier<'a> {
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        out.push_identifier(self.0)
    }
}

pub trait MiddleFragment {
    fn push_sql(&self, pass: AstPass<'_, '_>);
}

impl<'a> MiddleFragment for &'a str {
    fn push_sql(&self, mut pass: AstPass<'_, '_>) {
        pass.push_sql(self);
    }
}

#[derive(Debug, Copy, Clone)]
#[doc(hidden)] // used by the table macro
pub struct InfixNode<T, U, M> {
    lhs: T,
    rhs: U,
    middle: M,
}

impl<T, U, M> InfixNode<T, U, M> {
    #[doc(hidden)] // used by the table macro
    pub const fn new(lhs: T, rhs: U, middle: M) -> Self {
        InfixNode { lhs, rhs, middle }
    }
}

impl<T, U, M> QueryFragment for InfixNode<T, U, M>
where
    T: QueryFragment,
    U: QueryFragment,
    M: MiddleFragment,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        self.lhs.walk_ast(out.reborrow())?;
        self.middle.push_sql(out.reborrow());
        self.rhs.walk_ast(out.reborrow())?;
        Ok(())
    }
}
