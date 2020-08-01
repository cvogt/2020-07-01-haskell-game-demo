module Playtime
  ( module Playtime,
    module Playtime.EngineConfig,
    module Playtime.EngineState,
    module Playtime.Event,
    module Playtime.Geometry,
    module Playtime.LiveCode,
    module Playtime.Random,
    module Playtime.SaveLoad,
    module Playtime.Texture,
    module Playtime.UI,
    module Playtime.Util,
    module Playtime.Wiring,
    -- GLFW re-exports
    Key (..),
    KeyState (..),
    MouseButton (..),
    MouseButtonState (..),
  )
where

import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.IO
import My.Prelude
import Playtime.ConcurrentState
import Playtime.Debug
import Playtime.EngineConfig
import Playtime.EngineState
import Playtime.Event
import Playtime.GL
import Playtime.GLFW
import Playtime.Geometry
import Playtime.LiveCode
import Playtime.Random
import Playtime.SaveLoad
import Playtime.Texture
import Playtime.UI
import Playtime.Util
import Playtime.Wiring

-- README
-- Acronyms to know:
-- es = engine state
-- gs = game state
-- cs = concurrent state
-- pos = Position
-- dim = Dimension

playtime :: Either LiveCodeState (MVar EngineConfig) -> IO ()
playtime lcsOrEcMVar = do
  let (lcsMay, ecMVar) = either (\lcs -> (Just lcs, lcsEngineConfig lcs)) (Nothing,) lcsOrEcMVar
  EngineConfig {ecScale, ecDim, ecCheckIfContinue} <- readMVar ecMVar
  -- initialization
  ies@EngineState {esWindowSize} <- makeInitialEngineState ecScale ecDim <$> getSystemTime
  cs@ConcurrentState {..} <- makeInitialConcurrentState ies

  void $ forkDebugTerminal cs ecMVar lcsMay

  -- open gl rendering loop
  withGLFW esWindowSize "Playtime" $ \window -> do
    setEventCallback window $ void . stepStates ecMVar window cs

    whileM $ trackTimeM csTimeRender $ do
      GLFW.pollEvents
      EngineConfig {ecVisualize} <- readMVar ecMVar
      es <- stepStates ecMVar window cs . RenderEvent =<< getSystemTime
      pure es
        >>= ecVisualize
        >>= trackTimeM csTimeGL . renderGL window ecDim
      ecCheckIfContinue es
  where
    stepStates :: MVar EngineConfig -> GLFW.Window -> ConcurrentState -> Event -> IO EngineState
    stepStates ecMVar window ConcurrentState {..} event =
      modifyMVar csEngineState $ \old_es ->
        trackTimeM csTimeStep $ do
          EngineConfig {ecStepGameState} <- readMVar ecMVar
          GLFW.makeContextCurrent $ Just window -- needed in order to load textures in event handler threads
          let new_es = stepEngineState old_es event
          ecStepGameState new_es event
          pure (new_es, new_es)
