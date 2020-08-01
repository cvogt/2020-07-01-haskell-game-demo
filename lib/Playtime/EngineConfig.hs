module Playtime.EngineConfig where

import My.IO
import My.Prelude
import Playtime.EngineState
import Playtime.Event
import Playtime.Geometry
import Playtime.Texture

data EngineConfig = EngineConfig
  { ecStepGameState :: EngineState -> Event -> IO (),
    ecVisualize :: EngineState -> IO [Sprite],
    ecDim :: Dim,
    ecScale :: Double,
    ecCheckIfContinue :: EngineState -> IO Bool,
    ecGameDebugInfo :: EngineState -> IO [[Char]]
  }
