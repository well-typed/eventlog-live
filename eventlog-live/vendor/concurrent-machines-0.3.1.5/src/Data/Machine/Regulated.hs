{- HLINT ignore -}
{- FOURMOLU_DISABLE -}
-- | Slow producers down to run at desired rates.
module Data.Machine.Regulated where
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Machine.Plan
import Data.Machine.Process
import Data.Machine.Type
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- | A pass-through process rate-limited to the given inter-step
-- period in seconds. This may be used to slow down an upstream
-- producer; it can not speed things up.
regulated :: MonadIO m => Double -> ProcessT m a a
regulated target = construct $ liftIO getCurrentTime >>= go 0
  where go dt prevT =
          do await >>= \x -> yield x
             t <- liftIO getCurrentTime
             let e = target - realToFrac (diffUTCTime t prevT)
                 dt' = dt + 0.5 * e
             when (dt' > 0) (liftIO . threadDelay . round $ dt' * 1000000)
             go dt' t
