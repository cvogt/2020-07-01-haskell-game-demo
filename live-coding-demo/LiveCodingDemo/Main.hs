module LiveCodingDemo.Main where

import Codec.Picture (readPng)
import LiveCodingDemo.Game
import My.IO
import My.Prelude
import Playtime
import System.Random

gameDir :: FilePath
gameDir = "live-coding-demo"

main :: IO ()
main =
  playtime . Left
    =<< makeLiveCodeState makeEngineConfig "LiveCodingDemo.Main" "makeEngineConfig" (gameDir </> "LiveCodingDemo")

makeEngineConfig :: Maybe LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  initialGameState
    >>= wireEngineConfig
      (stepGameState . textureDim textures)
      (visualize . textureSprites textures)
      dimensions
      1
      liveCodeState
      loadTexture
      (snd . textures <$> allEnumValues)
  where
    dimensions = (1024, 768)
    initialGameState = makeInitialGameState dimensions <$> randomIO
    stepGameState area es@EngineState {..} old_gs event = do
      seed <- randomIO
      let new_gs = stepGameStatePure seed area old_gs es event
      if Key'R `setMember` esKeysPressed
        then initialGameState
        else pure new_gs
    loadTexture = \name -> either fail pure =<< (readPng $ gameDir </> "assets" </> name)
