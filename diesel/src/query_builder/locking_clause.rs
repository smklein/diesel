use crate::backend::Backend;
use crate::query_builder::{AstPass, QueryFragment, QueryId};
use crate::result::QueryResult;

#[derive(Debug, Clone, Copy, QueryId)]
pub enum AllLockingClauses {
    NoLocking,
    Locking(LockMode, LockModifier),
}

impl<DB> QueryFragment<DB> for AllLockingClauses
where
    DB: Backend,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b, DB>) -> QueryResult<()> {
        match self {
            AllLockingClauses::NoLocking => (),
            AllLockingClauses::Locking(mode, modifier) => {
                mode.walk_ast(out.reborrow())?;
                modifier.walk_ast(out.reborrow())?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, QueryId)]
pub enum LockMode {
    Update,
    #[cfg(feature = "postgres")]
    NoKeyUpdate,
    Share,
    #[cfg(feature = "postgres")]
    KeyShare,
}

impl<DB> QueryFragment<DB> for LockMode
where
    DB: Backend,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b, DB>) -> QueryResult<()> {
        match self {
            LockMode::Update => out.push_sql(" FOR UPDATE"),
            #[cfg(feature = "postgres")]
            LockMode::NoKeyUpdate => out.push_sql(" FOR NO KEY UPDATE"),
            LockMode::Share => out.push_sql(" FOR SHARE"),
            #[cfg(feature = "postgres")]
            LockMode::KeyShare => out.push_sql(" FOR KEY SHARE"),
        };
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, QueryId)]
pub enum LockModifier {
    NoModifier,
    SkipLocked,
    NoWait,
}

impl<DB> QueryFragment<DB> for LockModifier
where
    DB: Backend,
{
    fn walk_ast<'b>(&'b self, mut out: AstPass<'_, 'b, DB>) -> QueryResult<()> {
        match self {
            LockModifier::NoModifier => (),
            LockModifier::SkipLocked => out.push_sql(" SKIP LOCKED"),
            LockModifier::NoWait => out.push_sql(" NOWAIT"),
        };
        Ok(())
    }
}
