simple_clause!(NoOrderClause, OrderClause, " ORDER BY ");

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
