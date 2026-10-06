{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Machine.Validate
Description : Core machines for processing data in batches.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.Validate (
  -- * Validation
  validateInput,
  validateOrder,
  validateTicks,
) where

import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Foldable (for_)
import Data.Machine (ProcessT, await, construct)
import Data.Text qualified as T
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import GHC.Eventlog.Live.Machine.Core (Tick (..), TickInfo (..))
import GHC.Eventlog.Live.Types.Severity (Severity (..))
import Text.Printf (printf)

-------------------------------------------------------------------------------
-- Validation
--
-- TODO: These machines, or at least the error messages that they print, are
--       specific to eventlog processing. Hence, they should be moved.
-------------------------------------------------------------------------------

{- |
This machine validates that there is some input.

If no input is encountered after the given number of ticks, the machine prints
a warning that directs the user to check that the @-l@ flag was set correctly.
-}
validateInput ::
  (Monad m) =>
  Logger m ->
  Int ->
  ProcessT m (Tick a) x
validateInput logger ticks = construct $ start ticks
 where
  start remaining
    | remaining <= 0 = do
        let msg = printf "No input after %d ticks. Did you pass -l to the GHC RTS?" ticks
        lift $ writeLog logger WARN $ T.pack msg
        pure ()
    | otherwise = do
        let msg = "Waiting for " <> T.pack (show remaining) <> " more ticks before showing input warning."
        lift $ writeLog logger DEBUG $ msg
        await >>= \case
          Item{} ->
            lift $ writeLog logger DEBUG $ "Received item. Cancelled input warning."
          Tick ->
            start (pred remaining)

{- |
This machine validates that the inputs are received in order.

If an out-of-order input is encountered, the machine prints an error message
that directs the user to check that the @--eventlog-flush-interval@ flag is
set correctly.
-}
validateOrder ::
  (Monad m, Ord k, Show a) =>
  Logger m ->
  (a -> k) ->
  ProcessT m a x
validateOrder logger timestamp = construct $ go Nothing
 where
  go maybeOld =
    await >>= \new ->
      case maybeOld of
        Just old
          | timestamp new < timestamp old -> do
              let msg1 =
                    "Encountered two out-of-order inputs.\n\
                    \Did you pass --eventlog-flush-interval=SECONDS to the GHC RTS?\n\
                    \Did you pass the same flag to this program?"
              lift $ writeLog logger ERROR $ T.pack msg1
              let msg2 =
                    printf
                      "Out-of-order inputs:\n\
                      \- %s\n\
                      \- %s"
                      (show old)
                      (show new)
              lift $ writeLog logger DEBUG $ T.pack msg2
        _otherwise -> do
          go (Just new)

{- |
This machine validates that ticks are unique and increasing.
-}
validateTicks ::
  (Monad m) =>
  Logger m ->
  ProcessT m (Tick a) (Tick a)
validateTicks logger = construct $ go Nothing
 where
  go maybeTick =
    await >>= \case
      Item _ ->
        go maybeTick
      TickWithInfo{tickInfo = TickInfo{tick = tick'}} -> do
        for_ maybeTick $ \case
          tick
            | tick' == tick + 1 -> do
                let msg = "Saw tick " <> T.pack (show tick) <> "."
                lift $ writeLog logger TRACE $ msg
            | otherwise -> do
                let msg = "Encountered non-increasing ticks " <> T.pack (show tick) <> " and " <> T.pack (show tick') <> "."
                lift $ writeLog logger ERROR $ msg
        go (Just tick')
