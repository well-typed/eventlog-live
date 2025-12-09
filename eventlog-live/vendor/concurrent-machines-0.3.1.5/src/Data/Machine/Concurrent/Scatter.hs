{- HLINT ignore -}
{- FOURMOLU_DISABLE -}
{-# LANGUAGE FlexibleContexts, GADTs, TupleSections, RankNTypes,
             ScopedTypeVariables #-}
-- | Routing for splitting and merging processing pipelines.
module Data.Machine.Concurrent.Scatter (
  scatter, mergeSum, splitSum, splitProd
  ) where
import Control.Arrow ((***))
import Control.Concurrent.Async (Async, waitAny)
import Control.Concurrent.Async.Lifted (wait, waitEither, waitBoth)
import Control.Monad ((>=>))
import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Control (MonadBaseControl, restoreM, StM)
import Data.Machine
import Data.Machine.Concurrent.AsyncStep

holes :: [a] -> [[a]]
holes = go id
  where go _ [] = []
        go x (y:ys) = x ys : go (x . (y:)) ys

diff :: [a] -> [(a,[a])]
diff xs = zip xs (holes xs)

waitAnyHole :: MonadBaseControl IO m => [(Async (StM m a), [b])] -> m (a, [b])
waitAnyHole xs = do (_,(s,b)) <- liftBase $ waitAny xs'
                    fmap (,b) (restoreM s)
  where xs' = map (\(a,b) -> fmap (,b) a) xs

-- | Produces values from whichever source 'MachineT' yields
-- first. This operation may also be viewed as a /gather/ operation in
-- that all values produced by the given machines are interleaved when
-- fed downstream. Note that inputs are /not/ shared. The composite
-- machine will await an input when any constituent machine awaits an
-- input. That input will be supplied to the awaiting constituent and
-- no other.
--
-- Some examples of more specific useful types @scatter@ may be used
-- at,
--
-- @
-- scatter :: [ProcessT m a b] -> ProcessT m a b
-- scatter :: [SourceT m a] -> SourceT m a
-- @
--
-- The former may be used to stream data through a collection of
-- worker 'Process'es, the latter may be used to intersperse values
-- from a collection of sources.
scatter :: MonadBaseControl IO m => [MachineT m k o] -> MachineT m k o
scatter [] = stopped
scatter sinks = MachineT $ mapM asyncRun sinks
                 >>= waitAnyHole . diff
                 >>= uncurry go
  where go :: MonadBaseControl IO m
           => MachineStep m k o
           -> [AsyncStep m k o]
           -> m (MachineStep m k o)
        go Stop [] = return Stop
        go Stop sinks' = waitAnyHole (diff sinks') >>= uncurry go
        go (Yield o k) sinks' =
          asyncRun k >>= return . Yield o . MachineT . goWait . (:sinks')
        go (Await f fk ff) sinks' =
          asyncAwait f fk ff (MachineT . goWait . (:sinks'))
        goWait :: MonadBaseControl IO m
               => [AsyncStep m k o]
               -> m (MachineStep m k o)
        goWait = waitAnyHole . diff >=> uncurry go

-- | Similar to 'Control.Arrow.|||': split the input between two
-- processes and merge their outputs.
--
-- Connect two processes to the downstream tails of a 'Machine' that
-- produces 'Either's. The two downstream consumers are run
-- concurrently when possible. When one downstream consumer stops, the
-- other is allowed to run until it stops or the upstream source
-- yields a value the remaining consumer can not handle.
--
-- @mergeSum sinkL sinkR@ produces a topology like this,
--
-- @
--                                 sinkL
--                                /      \\
--                              a          \\
--                             /            \\
--    source -- Either a b -->                -- r -->
--                             \\            /
--                              b          /
--                               \\       /
--                                 sinkR
-- @
mergeSum :: forall m a b r. MonadBaseControl IO m
         => ProcessT m a r -> ProcessT m b r -> ProcessT m (Either a b) r
mergeSum snkL snkR = MachineT $ do sl <- asyncRun snkL
                                   sr <- asyncRun snkR
                                   go sl sr
  where go :: AsyncStep m (Is a) r
           -> AsyncStep m (Is b) r
           -> m (MachineStep m (Is (Either a b)) r)
        go sl sr = waitEither sl sr >>=
                   \(s :: Either (MachineStep m (Is a) r)
                                 (MachineStep m (Is b) r)) -> case s of
          Left Stop -> wait sr >>= runMachineT . rightOnly . encased
          Right Stop -> wait sl >>= runMachineT . leftOnly . encased

          Left (Yield o k) ->
            return . Yield o . MachineT $ asyncRun k >>= flip go sr
          Right (Yield o k) ->
            return . Yield o . MachineT $ asyncRun k >>= go sl

          Left (Await f Refl ff) ->
            return $
            Await (\u -> case u of
                           Left a -> MachineT $ asyncRun (f a) >>= flip go sr
                           Right b -> MachineT $
                                      wait sr >>= forceFeed (go sl) b . encased)
                  Refl
                  (MachineT $ asyncRun ff >>= flip go sr)
          Right (Await g Refl gg) -> return $
            Await (\u -> case u of
                           Left a ->
                             MachineT $
                             wait sl >>= forceFeed (flip go sr) a . encased
                           Right b -> MachineT $ asyncRun (g b) >>= go sl)
                  Refl
                  (MachineT $ asyncRun gg >>= go sl)

-- | Similar to 'Control.Arrow.+++': split the input between two
-- processes, retagging and merging their outputs.
--
-- The two processes are run concurrently whenever possible.
splitSum :: forall m a b c d. MonadBaseControl IO m
         => ProcessT m a b -> ProcessT m c d -> ProcessT m (Either a c) (Either b d)
splitSum snkL snkR = MachineT $ do sl <- asyncRun (fmap lft snkL)
                                   sr <- asyncRun (fmap rgt snkR)
                                   go sl sr
  where lft :: b -> Either b d
        lft = Left
        rgt :: d -> Either b d
        rgt = Right
        go :: AsyncStep m (Is a) (Either b d)
           -> AsyncStep m (Is c) (Either b d)
           -> m (MachineStep m (Is (Either a c)) (Either b d))
        go sl sr = waitEither sl sr >>=
                   \(s :: Either (MachineStep m (Is a) (Either b d))
                                 (MachineStep m (Is c) (Either b d))) -> case s of
          Left Stop -> wait sr >>= runMachineT . rightOnly . encased
          Right Stop -> wait sl >>= runMachineT . leftOnly . encased

          Left (Yield o k) ->
            return . Yield o . MachineT $ asyncRun k >>= flip go sr
          Right (Yield o k) ->
            return . Yield o . MachineT $ asyncRun k >>= go sl

          Left (Await f Refl ff) ->
            return $
            Await (\u -> case u of
                           Left a -> MachineT $ asyncRun (f a) >>= flip go sr
                           Right b -> MachineT $
                                      wait sr >>= forceFeed (go sl) b . encased)
                  Refl
                  (MachineT $ asyncRun ff >>= flip go sr)
          Right (Await g Refl gg) -> return $
            Await (\u -> case u of
                           Left a ->
                             MachineT $
                             wait sl >>= forceFeed (flip go sr) a . encased
                           Right b -> MachineT $ asyncRun (g b) >>= go sl)
                  Refl
                  (MachineT $ asyncRun gg >>= go sl)

-- | @forceFeed k x p@ runs machine @p@ until it awaits, at which
-- point it is fed @x@. The result of that feeding is asynchronously
-- run, and supplied to the continuation @k@.
forceFeed :: forall m a k b. MonadBaseControl IO m
          => (AsyncStep m (Is a) b -> m (MachineStep m k b))
          -> a
          -> ProcessT m a b
          -> m (MachineStep m k b)
forceFeed go x = aux
  where aux p = runMachineT p >>= \v -> case v of
          -- Stop -> asyncRun stopped >>= go
          Stop -> return Stop
          Yield o k -> return . Yield o . MachineT $ aux k
          Await f Refl _ -> asyncRun (f x) >>= go

-- | We have a sink for the Right output of a source, so we want to
-- keep running it as long as upstream does not yield a 'Left' which
-- we can not handle. When upstream yields a 'Left', we 'stop'.
rightOnly :: Monad m => ProcessT m b r -> ProcessT m (Either a b) r
rightOnly snk = repeatedly (await >>= either (const stop) (\x -> yield x)) ~> snk

-- | We have a sink for the Left output of a source, so we want to
-- keep running it as long as upstream does not yield a 'Right' which
-- we can not handle. When upstream yields a 'Right', we 'stop'.
leftOnly :: Monad m => ProcessT m a r -> ProcessT m (Either a b) r
leftOnly snk = repeatedly (await >>= either (\x -> yield x) (const stop)) ~> snk

-- | Connect two processes to the downstream tails of a 'Machine' that
-- produces tuples. The two downstream consumers are run
-- concurrently. When one downstream consumer stops, the entire
-- pipeline is stopped.
--
-- @splitProd sink1 sink2@ produces a topology like this,
--
-- @
--                            sink1
--                           /      \\
--                         a          \\
--                        /            \\
--    source -- (a,b) -->               -- r -->
--                        \\            /
--                         b         /
--                           \\     /
--                            sink2
-- @
splitProd :: forall m a b r. MonadBaseControl IO m
          => ProcessT m a r -> ProcessT m b r -> ProcessT m (a,b) r
splitProd snk1 snk2 = MachineT $ do s1 <- asyncRun snk1
                                    s2 <- asyncRun snk2
                                    go s1 s2
  where go :: AsyncStep m (Is a) r
           -> AsyncStep m (Is b) r
           -> m (MachineStep m (Is (a,b)) r)
        go s1 s2 = waitBoth s1 s2 >>=
                   \(ss :: (MachineStep m (Is a) r, MachineStep m (Is b) r)) -> case ss of
          (Stop, _) -> return Stop
          (_, Stop) -> return Stop
          (Yield o1 k1, Yield o2 k2) ->
            return . Yield o1 . encased $ Yield o2 $ MachineT $
            do k1' <- asyncRun k1
               k2' <- asyncRun k2
               go k1' k2'
          (Yield o k, _) ->
            return . Yield o . MachineT $ asyncRun k >>= flip go s2
          (_, Yield o k) ->
            return . Yield o . MachineT $ asyncRun k >>= go s1
          (Await f Refl ff, Await g Refl gg) ->
            return $ Await (uncurry splitProd . (f***g)) Refl (splitProd ff gg)
