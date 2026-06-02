module GHC.Eventlog.Live.Source.Core (
  EventlogSourceOptions (..),
  EventlogSocketAddr (..),
  EventlogSourceHandle (..),
  EventlogSourceData (..),
  recv,
) where

import Control.Exception (handle, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Network.Socket (Socket)
import Network.Socket.ByteString qualified as SB
import System.IO (Handle)
import System.IO qualified as IO (stdin)
import System.IO.Error (isEOFError)
import System.Timeout (timeout)

{- |
The options for different kinds of eventlog sources.
-}
data EventlogSourceOptions
  = EventlogSourceOptionsStdin
  | EventlogSourceOptionsFile FilePath
  | EventlogSourceOptionsSocket EventlogSocketAddr

{- |
The options for different kinds of eventlog sockets.
-}
data EventlogSocketAddr
  = EventlogSocketUnixAddr
      { esaUnixPath :: FilePath
      {- ^ Unix socket path, e.g., @"\/tmp\/ghc_eventlog.sock"@.

      __Warning:__ Unix domain socket paths are often limited to 107 characters or less.
      -}
      }
  | EventlogSocketInetAddr
      { esaInetHost :: String
      -- ^ TCP host or interface, e.g. @"127.0.0.1"@.
      , esaInetPort :: String
      -- ^ TCP port, e.g., @"4242"@.
      }
  deriving (Eq, Show)

{- |
The handles for different kinds of eventlog sources.
-}
data EventlogSourceHandle
  = EventlogSourceHandleStdin
  | EventlogSourceHandleFile Handle
  | EventlogSourceHandleSocket Socket

data EventlogSourceData
  = EventlogSourceData ByteString
  | EventlogSourceTimeout
  | EventlogSourceClosed

{- |
Receive data from an `EventlogSourceHandle`.

__Warning__: The current implementation is blocking on Windows. See the documentation for `timeout`.
-}
recv ::
  -- | The handle to the eventlog source.
  EventlogSourceHandle ->
  -- | The timeout in microseconds.
  Int ->
  -- | The number of bytes to read.
  Int ->
  IO EventlogSourceData
recv = \case
  EventlogSourceHandleStdin -> recvFromHandle IO.stdin
  EventlogSourceHandleFile h -> recvFromHandle h
  EventlogSourceHandleSocket s -> recvFromSocket s

-- Permit a timeout and wrap the result appropriately.

{- |
Internal helper.
Receive data from a `Socket`.
-}
recvFromSocket ::
  -- | The eventlog socket.
  Socket ->
  -- | The timeout in microseconds.
  Int ->
  -- | The number of bytes to read.
  Int ->
  IO EventlogSourceData
recvFromSocket s timeoutMicros chunkSizeBytes =
  withTimeout timeoutMicros $ do
    msg <- SB.recv s chunkSizeBytes
    if BS.null msg
      then pure EventlogSourceClosed
      else pure $ EventlogSourceData msg

{- |
Internal helper.
Receive data from a `Handle`.
-}
recvFromHandle ::
  -- | The handle to the eventlog source.
  Handle ->
  -- | The timeout in microseconds.
  Int ->
  -- | The number of bytes to read.
  Int ->
  IO EventlogSourceData
recvFromHandle h timeoutMicros chunkSizeBytes =
  withTimeout timeoutMicros $
    handleEOFError EventlogSourceClosed $
      EventlogSourceData <$> BS.hGetSome h chunkSizeBytes

{- |
Internal helper.
Allow the IO operation to timeout.
-}
withTimeout ::
  -- | The timeout in microseconds.
  Int ->
  -- | The IO action to read data from the eventlog source.
  IO EventlogSourceData ->
  IO EventlogSourceData
withTimeout timeoutMicros =
  fmap (fromMaybe EventlogSourceTimeout) . timeout timeoutMicros

{- |
Internal helper.
Recover from an EOF error with a default value.
-}
handleEOFError :: a -> IO a -> IO a
handleEOFError d = handle (\e -> if isEOFError e then pure d else throwIO e)
