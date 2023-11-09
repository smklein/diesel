macro_rules! simple_optional_clause {
    (
        $(#[$clause_meta: meta])*
        $clause:ident,
        $sql:expr
    ) => {
        use crate::result::QueryResult;
        use crate::query_builder::QueryId;
        use super::{AstPass, QueryFragment};

        $(#[$clause_meta])*
        #[derive(QueryId)]
        #[allow(missing_debug_implementations)]
        pub enum $clause {
            None,
            Some(Box<dyn QueryFragment + Send>),
        }

        impl $clause {
            pub fn new<Expr: 'static>(expr: Expr) -> Self
            where
                Expr: QueryFragment + Send,
            {
                Self::Some(Box::new(expr))
            }
        }

        impl QueryFragment for $clause
        {
            fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b>) -> QueryResult<()>
            {
                match self {
                    $clause::None => (),
                    $clause::Some(expr) => {
                        out.push_sql($sql);
                        expr.walk_ast(out.reborrow())?;
                    }
                }
                Ok(())
            }
        }
    }
}
