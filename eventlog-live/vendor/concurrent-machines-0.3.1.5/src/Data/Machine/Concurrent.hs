{- HLINT ignore -}
{- FOURMOLU_DISABLE -}
{-# LANGUAGE CPP, GADTs, FlexibleContexts, RankNTypes, ScopedTypeVariables,
             TupleSections #-}
-- | The primary use of concurrent machines is to establish a
-- pipelined architecture that can boost overall throughput by running
-- each stage of the pipeline at the same time. The processing, or
-- production, rate of each stage may not be identical, so facilities
-- are provided to loosen the temporal coupling between pipeline
-- stages using buffers.
--
-- This architecture also lends itself to operations where multiple
-- workers are available for procesisng inputs. If each worker is to
-- process the same set of inputs, consider 'fanout' and
-- 'fanoutSteps'. If each worker is to process a disjoint set of
-- inputs, consider 'scatter'.
module Data.Machine.Concurrent (module Data.Machine,
                                -- * Concurrent connection
                                (>~>), (<~<),
                                -- * Buffered machines
                                bufferConnect, rollingConnect,
                                -- * Concurrent processing of shared inputs
                                fanout, fanoutSteps,
                                -- * Concurrent multiple-input machines
                                wye, tee, scatter, splitSum, mergeSum,
                                splitProd) where
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Concurrent.Async.Lifted
import Control.Monad (join)
import Control.Monad.Trans.Control
import Data.Machine hiding (tee, wye)
import Data.Machine.Concurrent.AsyncStep
import Data.Machine.Concurrent.Buffer
import Data.Machine.Concurrent.Fanout
import Data.Machine.Concurrent.Scatter
import Data.Machine.Concurrent.Wye
import Data.Machine.Concurrent.Tee

-- | Build a new 'Machine' by adding a 'Process' to the output of an
-- old 'Machine'. The upstream machine is run concurrently with
-- downstream with the aim that upstream will have a yielded value
-- ready as soon as downstream awaits. This effectively creates a
-- buffer between upstream and downstream, or source and sink, that
-- can contain up to one value.
--
-- @
-- ('<~<') :: 'Process' b c -> 'Process' a b -> 'Process' a c
-- ('<~<') :: 'Process' c d -> 'Data.Machine.Tee.Tee' a b c -> 'Data.Machine.Tee.Tee' a b d
-- ('<~<') :: 'Process' b c -> 'Machine' k b -> 'Machine' k c
-- @
(<~<) :: MonadBaseControl IO m
     => ProcessT m b c -> MachineT m k b -> MachineT m k c
mp <~< ma = racers ma mp

-- | Flipped ('<~<').
(>~>) :: MonadBaseControl IO m
     => MachineT m k b -> ProcessT m b c -> MachineT m k c
ma >~> mp = mp <~< ma

infixl 7 >~>

-- | We want the first available response.
waitEither' :: MonadBaseControl IO m
            => Maybe (Async (StM m a)) -> Async (StM m b)
            -> m (Either a b)
waitEither' Nothing y = Right <$> wait y
waitEither' (Just x) y = waitEither x y

-- | Let a source and a sink chase each other, providing an effective
-- one-element buffer between the two. The idea is to run both
-- concurrently at all times so that as soon as the sink 'Await's, we
-- have a source-yielded value to provide it. This, of course,
-- involves eagerly running the source, percolating its 'Await's up
-- the chain as soon as possible.
racers :: forall m k a b. MonadBaseControl IO m
       => MachineT m k a -> ProcessT m a b -> MachineT m k b
racers src snk = MachineT . join $
                 go <$> (Just <$> asyncRun src) <*> asyncRun snk
  where go :: Maybe (AsyncStep m k a)
           -> AsyncStep m (Is a) b
           -> m (MachineStep m k b)
        go srcA snkA =
          waitEither' srcA snkA >>= \n -> case n of
            Left (Stop :: MachineStep m k a) -> go Nothing snkA
            Left (Yield o k) -> wait snkA >>= \m -> case m of
              (Stop :: MachineStep m (Is a) b) -> return Stop
              Yield o' k' -> return . Yield o' . MachineT . flushDown k' $
                             \f -> join $ go <$> (Just <$> asyncRun k)
                                             <*> asyncRun (f o)
              Await f Refl _ -> join $ go <$> (Just <$> asyncRun k)
                                          <*> asyncRun (f o)
            Left (Await g kg fg) -> asyncAwait g kg fg $
                                    MachineT . flip go snkA . Just
            Right (Stop :: MachineStep m (Is a) b) -> return Stop
            Right (Yield o k) -> asyncRun k >>=
                                 return . Yield o . MachineT . go srcA
            Right (Await f Refl ff) -> case srcA of
              Nothing -> asyncRun ff >>= go Nothing
              Just src' -> wait src' >>= \m -> case m of
                Stop -> return Stop
                Yield o k -> join $ go <$> (Just <$> asyncRun k)
                                       <*> asyncRun (f o)
                a -> feedUp (encased a) $ \o k -> join $
                       go <$> (Just <$> asyncRun k) <*> asyncRun (f o)
        -- If we have an upstream source value ready, we must flush
        -- all available values yielded by downstream until it awaits.
        flushDown :: ProcessT m a b
                  -> ((a -> ProcessT m a b) -> m (MachineStep m k b))
                  -> m (MachineStep m k b)
        flushDown m k = runMachineT m >>= \s -> case s of
          Stop -> return Stop
          Yield o m' -> return . Yield o . MachineT $ flushDown m' k
          Await f Refl _ -> k f
        -- If downstream is awaiting an input, we must pull in all
        -- necessary upstream awaits until we have a yielded value to
        -- push downstream.
        feedUp :: MachineT m k a
               -> (a -> MachineT m k a -> m (MachineStep m k b))
               -> m (MachineStep m k b)
        feedUp m k = runMachineT m >>= \s -> case s of
          Stop -> return Stop
          Yield o m' -> k o m'
          Await g kg fg -> return $ awaitStep g kg fg (MachineT . flip feedUp k)
