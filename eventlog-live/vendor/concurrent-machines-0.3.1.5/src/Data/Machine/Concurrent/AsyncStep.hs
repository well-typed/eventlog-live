{- HLINT ignore -}
{- FOURMOLU_DISABLE -}
{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, ScopedTypeVariables #-}
-- | Internal helpers for taking asynchronous machine steps.
module Data.Machine.Concurrent.AsyncStep where
import Control.Concurrent.Async.Lifted (Async, async, wait)
import Control.Monad.Trans.Control (MonadBaseControl, StM)
import Data.Machine

-- | Slightly more compact notation for a 'Step'.
type MachineStep m k o = Step k o (MachineT m k o)

-- | Compact notation for a 'Step' taken asynchronously.
type AsyncStep m k o = Async (StM m (MachineStep m k o))

-- | Build an 'Await' step given a continuation that provides
-- subsequent steps. @awaitStep f sel ff k@ is like applying the
-- 'Await' constructor directly, but the continuation @k@ is used to
-- continue the machine.
--
-- @awaitStep f sel ff k = Await (k . f) sel (k ff)@
awaitStep :: (a -> d) -> k' a -> d -> (d -> r) -> Step k' b r
awaitStep f sel ff k = Await (k . f) sel (k ff)

-- | Run one step of a machine as an 'Async' operation.
asyncRun :: MonadBaseControl IO m => MachineT m k o -> m (AsyncStep m k o)
asyncRun = async . runMachineT

-- | Satisfy a downstream Await by blocking on an upstream step.
stepAsync :: forall m k k' a' d b.
             MonadBaseControl IO m
           => (forall c. k c -> k' c)
           -> AsyncStep m k a'
           -> (a' -> d)
           -> d
           -> d
           -> (AsyncStep m k a' -> d -> MachineT m k' b)
           -> MachineT m k' b
stepAsync sel src f def prev go = MachineT $ wait src >>= \u -> case u of
  Stop -> go' stopped def
  Yield a k -> go' k (f a)
  Await g kg fg -> return $ awaitStep g (sel kg) fg (MachineT . flip go' prev)
  where go' :: MachineT m k a' -> d -> m (MachineStep m k' b)
        go' k d = asyncRun k >>= runMachineT . flip go d

-- | @asyncEncased f x@ launches @x@ and provides the resulting
-- 'AsyncStep' to @f@. Turn a function on 'AsyncStep' to a funciton on
-- 'MachineT'.
asyncEncased :: MonadBaseControl IO m
             => (AsyncStep m k1 o1 -> MachineT m k o)
             -> MachineT m k1 o1
             -> MachineT m k o
asyncEncased f x = MachineT $ asyncRun x >>= runMachineT . f

-- | Similar to 'awaitStep', but for continuations that want their inputs
-- to be run asynchronously.
asyncAwait :: MonadBaseControl IO m
           => (a -> MachineT m k o)
           -> k' a
           -> MachineT m k o
           -> (AsyncStep m k o -> MachineT m k1 o1)
           -> m (Step k' b (MachineT m k1 o1))
asyncAwait f sel ff = return . awaitStep f sel ff . asyncEncased
