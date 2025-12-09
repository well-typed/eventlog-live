{- HLINT ignore -}
{- FOURMOLU_DISABLE -}
 {-# LANGUAGE CPP, FlexibleContexts, GADTs, ScopedTypeVariables, TupleSections #-}
-- | Place buffers between two machines. This is most useful with
-- irregular production rates.
module Data.Machine.Concurrent.Buffer (
  -- * Blocking buffers
  bufferConnect,
  -- * Non-blocking (rolling) buffers
  rollingConnect,
  -- * Internal helpers
  mediatedConnect, BufferRoom(..)
  ) where
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Concurrent.Async.Lifted (wait, waitEither)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad (join, (>=>))
import Data.Machine.Concurrent.AsyncStep
import Data.Machine
import Data.Sequence (ViewL(..), (|>))
import qualified Data.Sequence as S
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Data.Traversable (traverse)
#endif

-- | Drain downstream until it awaits a value, then pass the awaiting
-- step to the given function.
drain :: Monad m
      => MachineStep m k a
      -> (MachineStep m k a -> m (MachineStep m k' a))
      -> m (MachineStep m k' a)
drain z k = go z
  where go Stop = return Stop
        go (Yield o kd) = Yield o . MachineT . go <$> runMachineT kd
        go aStep = k aStep

-- | Feed upstream until it yields a value, then pass the yielded
-- value and next step to the given function.
feedToBursting :: Monad m
               => MachineStep m k a
               -> (Maybe (a, MachineT m k a) -> m (MachineStep m k b))
               -> m (MachineStep m k b)
feedToBursting z k = go z
  where go Stop = k Nothing
        go (Await f kf ff) = return $
          Await (\a -> go' (f a)) kf (go' ff)
        go (Yield o kk) = k $ Just (o, kk)
        go' step = MachineT $ runMachineT step >>= go

-- | Mediate a 'MachineT' and a 'ProcessT' with a bounded capacity
-- buffer. The source machine runs concurrently with the sink process,
-- and is only blocked when the buffer is full.
bufferConnect :: MonadBaseControl IO m
              => Int -> MachineT m k b -> ProcessT m b c -> MachineT m k c
bufferConnect n = mediatedConnect S.empty snoc view
  where snoc acc x = (if S.length acc < n - 1 then Vacancy else NoVacancy) $
                       acc |> x
        view acc = case S.viewl acc of
                     EmptyL -> Nothing
                     x :< acc' -> Just (x, acc')

-- | Mediate a 'MachineT' and a 'ProcessT' with a rolling buffer. The
-- source machine runs concurrently with the sink process and is never
-- blocked. If the sink process can not keep up with upstream, yielded
-- values will be dropped.
rollingConnect :: MonadBaseControl IO m
              => Int -> MachineT m k b -> ProcessT m b c -> MachineT m k c
rollingConnect n = mediatedConnect S.empty snoc view
  where snoc acc x = Vacancy $ S.take (n-1) acc |> x
        view acc = case S.viewl acc of
                     EmptyL -> Nothing
                     x :< acc' -> Just (x, acc')

-- | Indication if the payload value is "full" or not.
data BufferRoom a = NoVacancy a | Vacancy a deriving (Eq, Ord, Show)

-- | Mediate a 'MachineT' and a 'ProcessT' with a buffer.
--
-- @mediatedConnect z snoc view source sink@ pipes @source@ into
-- @sink@ through a buffer initialized to @z@ and updated with
-- @snoc@. Upstream is blocked if @snoc@ indicates that the buffer is
-- full after adding a new element. Downstream blocks if @view@
-- indicates that the buffer is empty. Otherwise, @view@ is expected
-- to return the next element to process and an updated buffer.
mediatedConnect :: forall m t b k c. MonadBaseControl IO m
                => t -> (t -> b -> BufferRoom t) -> (t -> Maybe (b,t))
                -> MachineT m k b -> ProcessT m b c -> MachineT m k c
mediatedConnect z snoc view src0 snk0 =
  MachineT $ do srcFuture <- asyncRun src0
                snkFuture <- asyncRun snk0
                go z (Just srcFuture) snkFuture
  where -- Wait for the next available step
        go :: t
           -> Maybe (AsyncStep m k b)
           -> AsyncStep m (Is b) c
           -> m (MachineStep m k c)
        go acc src snk = maybe (Left <$> wait snk) (waitEither snk) src >>=
                           goStep acc . either (Right . (,src)) (Left . (,snk))

        -- Kick off the next step of both the source and the sink
        goAsync :: t
                -> Maybe (MachineT m k b)
                -> ProcessT m b c
                -> m (MachineStep m k c)
        goAsync acc src snk =
          join $ go acc <$> traverse asyncRun src <*> asyncRun snk

        -- Handle whichever step is ready first
        goStep :: t  -> Either (MachineStep m k b, AsyncStep m (Is b) c)
                               (MachineStep m (Is b) c, Maybe (AsyncStep m k b))
               -> m (MachineStep m k c)
        goStep acc step = case step of
          -- @src@ stepped first
          Left (Stop, snk) -> go acc Nothing snk
          Left (Await g kg fg, snk) ->
            asyncAwait g kg fg (MachineT . flip (go acc) snk . Just)
          Left (Yield o k, snk) -> case snoc acc o of
            -- add it to the right end of the buffer
            Vacancy acc' -> asyncRun k >>= flip (go acc') snk . Just
            -- buffer was full
            NoVacancy acc' ->
              let go' snk' = do src' <- asyncRun k
                                goStep acc' (Right (snk', Just src'))
              in wait snk >>= flip drain go'

          -- @snk@ stepped first
          Right (Stop, _) -> return Stop
          Right (Yield o k, src) -> do
            return $ Yield o (MachineT $ asyncRun k >>= go acc src)
          Right (Await f Refl ff, src) ->
            case view acc of
              Nothing -> maybe (goAsync acc Nothing ff) (wait >=> demandSrc) src
              Just (x, acc') -> asyncRun (f x) >>= go acc' src
            where demandSrc = flip feedToBursting go'
                  go' Nothing = goAsync acc Nothing ff
                  go' (Just (o, k)) = goAsync acc (Just k) (f o)
