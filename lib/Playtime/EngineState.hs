module Playtime.EngineState where

import GHC.Float
import My.Prelude
import Playtime.Event
import Playtime.Geometry
import Playtime.Util

data EngineState = EngineState
  { esCursorPos :: Pos,
    esFps :: Double,
    esDimensions :: Dim,
    esKeysPressed :: Set Key,
    esMousePressed :: Set MouseButton,
    esLastLoopTime :: SystemTime,
    esActions :: Set Action,
    esTimes :: [Integer],
    esTimePassed :: Double,
    esWindowSize :: Dim
  }
  deriving (Show, Generic, NFData)

makeInitialEngineState :: Double -> Dim -> SystemTime -> EngineState
makeInitialEngineState scale dim time =
  EngineState
    { esCursorPos = 0,
      esFps = 0,
      esKeysPressed = mempty,
      esMousePressed = mempty,
      esLastLoopTime = time,
      esDimensions = dim,
      esActions = mempty,
      esTimes = [],
      esTimePassed = 0,
      esWindowSize = dupe scale * dim
    }

gameExitRequested :: EngineState -> Bool
gameExitRequested es = Exit `elem` (esActions es)

clearOneTimeEffects :: EngineState -> EngineState
clearOneTimeEffects es =
  es
    { esActions =
        -- clear triggers for one time side effects
        esActions es `difference` (setFromList $ fmap OneTimeEffect $ catMaybes $ fmap oneTimeEffectMay $ toList $ esActions es)
    }

stepEngineState :: EngineState -> Event -> EngineState
stepEngineState (clearOneTimeEffects -> gs@EngineState {..}) = \case
  WindowSizeEvent width height -> gs {esWindowSize = (int2Double width, int2Double height)}
  CursorPosEvent pos ->
    gs
      { esCursorPos =
          -- this ratio calculation leads to proper relative scaling on window resize
          -- FIXME: we still get distortion if aspect ration of resized window is different
          --        we should be able to fix that by adding black borders as needed
          let scale = esDimensions / esWindowSize
              relPos = pos
           in scale * relPos
      }
  KeyEvent key KeyState'Pressed ->
    let pressed = setInsert key esKeysPressed
        matchingBindings = fromMaybe [] $ mapLookup key $ groupKeyBindings keyBindings
        actions = setFromList $ take 1 $ snd <$> filter (null . (`difference` pressed) . fst) matchingBindings
     in gs {esKeysPressed = pressed, esActions = esActions `union` actions}
  KeyEvent key KeyState'Released ->
    let pressed = setInsert key esKeysPressed
        matchingBindings = fromMaybe [] $ mapLookup key $ groupKeyBindings keyBindings
        actions = setFromList $ take 1 $ snd <$> filter (null . (`difference` pressed) . fst) matchingBindings
     in gs {esKeysPressed = setDelete key esKeysPressed, esActions = esActions `difference` actions}
  MouseEvent mb MouseButtonState'Pressed -> gs {esMousePressed = setInsert mb esMousePressed}
  MouseEvent mb MouseButtonState'Released -> gs {esMousePressed = setDelete mb esMousePressed}
  WindowCloseEvent -> gs {esActions = setFromList [Exit, OneTimeEffect Save] `union` esActions}
  RenderEvent time ->
    let picosecs = timeDiffPico esLastLoopTime time
        halfsec = 500 * 1000 * 1000 * 1000
     in gs
          { esLastLoopTime = time,
            esTimePassed = pico2Double picosecs,
            esTimes = if sum esTimes > halfsec then [] else picosecs : esTimes,
            esFps = if sum esTimes > halfsec then avg esTimes else esFps
          }
  _ -> gs
