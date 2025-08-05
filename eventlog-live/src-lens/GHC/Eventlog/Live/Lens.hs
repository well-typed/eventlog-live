module GHC.Eventlog.Live.Lens where

import GHC.RTS.Events (Event (Event), EventInfo, Timestamp)
import GHC.RTS.Events qualified as E (Event (..))
import Lens.Family2 (Lens')
import Lens.Family2.Unchecked (lens)

evTime :: Lens' Event Timestamp
evTime = lens get set
 where
  get Event{E.evTime = x} = x
  set ev x = ev{E.evTime = x}

evSpec :: Lens' Event EventInfo
evSpec = lens get set
 where
  get Event{E.evSpec = x} = x
  set ev x = ev{E.evSpec = x}

evCap :: Lens' Event (Maybe Int)
evCap = lens get set
 where
  get Event{E.evCap = x} = x
  set ev x = ev{E.evCap = x}
