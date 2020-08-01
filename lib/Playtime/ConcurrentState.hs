module Playtime.ConcurrentState where

import My.IO
import My.Prelude
import Playtime.EngineState

data ConcurrentState = ConcurrentState
  { csEngineState :: MVar EngineState,
    csTimeStep :: MVar [(SystemTime, SystemTime)],
    csTimeGL :: MVar [(SystemTime, SystemTime)],
    csTimeRender :: MVar [(SystemTime, SystemTime)]
  }

makeInitialConcurrentState :: EngineState -> IO ConcurrentState
makeInitialConcurrentState es = do
  csEngineState <- newMVar es
  csTimeStep <- newMVar []
  csTimeGL <- newMVar []
  csTimeRender <- newMVar []
  pure $ ConcurrentState {..}
