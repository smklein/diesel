simple_clause!(NoOrderClause, OrderClause, " ORDER BY ");

#[allow(missing_debug_implementations)] // We can't...
pub enum BoxedOrderClause<'a> {
    Order(Box<dyn QueryFragment + Send + 'a>),
    None,
}

impl<'a> QueryFragment for BoxedOrderClause<'a> {
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()> {
        match *self {
            BoxedOrderClause::Order(ref order_clause) => {
                out.push_sql(" ORDER BY ");
                order_clause.walk_ast(out)
            }
            BoxedOrderClause::None => Ok(()),
        }
    }
}

impl<'a> QueryId for BoxedOrderClause<'a> {
    type QueryId = ();

    const HAS_STATIC_QUERY_ID: bool = false;
}

impl<'a, Expr> From<OrderClause<Expr>> for Option<Box<dyn QueryFragment + Send + 'a>>
where
    Expr: QueryFragment + Send + 'a,
{
    fn from(order: OrderClause<Expr>) -> Self {
        Some(Box::new(order.0))
    }
}

impl<'a> From<NoOrderClause> for Option<Box<dyn QueryFragment + Send + 'a>> {
    fn from(_: NoOrderClause) -> Self {
        None
    }
}
