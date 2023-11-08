macro_rules! simple_clause {
    (
        $(#[$no_clause_meta: meta])*
        $no_clause:ident,
        $(#[$clause_meta: meta])*
        $clause:ident,
        $sql:expr
    ) => {
        use crate::result::QueryResult;
        use crate::query_builder::QueryId;
        use super::{AstPass, DB, QueryFragment};

        $(#[$no_clause_meta])*
        #[derive(Debug, Clone, Copy, QueryId)]
        pub struct $no_clause;

        impl QueryFragment for $no_clause where {
            fn walk_ast<'b>(&'b self, _: AstPass<'_, 'b, DB>) -> QueryResult<()>
            {
                Ok(())
            }
        }

        $(#[$clause_meta])*
        #[derive(Debug, Clone, Copy, QueryId)]
        pub struct $clause<Expr>(pub Expr);

        impl<Expr> QueryFragment for $clause<Expr> where
            Expr: QueryFragment,
        {
            fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b, DB>) -> QueryResult<()>
            {
                out.push_sql($sql);
                self.0.walk_ast(out.reborrow())?;
                Ok(())
            }
        }
    }
}
