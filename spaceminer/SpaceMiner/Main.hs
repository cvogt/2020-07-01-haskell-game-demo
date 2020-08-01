module SpaceMiner.Main where

import Codec.Picture (readPng)
import My.IO
import My.Prelude
import Playtime
import SpaceMiner.GameState
import SpaceMiner.Visualize

gameDir :: FilePath
gameDir = "spaceminer"

main :: IO ()
main =
  playtime . Left
    =<< makeLiveCodeState makeEngineConfig "SpaceMiner.Main" "makeEngineConfig" (gameDir </> "SpaceMiner")

makeEngineConfig :: Maybe LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  wireEngineConfig
    (stepGameState . textureDim textures)
    (visualize . textureSprites textures)
    dimensions
    3
    liveCodeState
    loadTexture
    (snd . textures <$> allEnumValues)
    $ makeInitialGameState dimensions
  where
    dimensions = (320, 240)
    stepGameState area es@EngineState {..} old_gs event = do
      let new_gs = stepGameStatePure area old_gs es event
      saveMay es new_gs
      fromMaybe new_gs <$> loadMay es
    loadTexture = \name -> either fail pure =<< (readPng $ gameDir </> "assets" </> name)
