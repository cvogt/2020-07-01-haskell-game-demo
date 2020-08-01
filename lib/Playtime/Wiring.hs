module Playtime.Wiring where

import Codec.Picture (DynamicImage)
import Data.Aeson
import GHC.Err (error)
import My.IO
import My.Prelude
import Playtime.Debug
import Playtime.EngineConfig
import Playtime.EngineState
import Playtime.Event
import Playtime.GL
import Playtime.Geometry
import Playtime.LiveCode
import Playtime.Texture

wireEngineConfig ::
  forall a gs.
  (Ord a, Show a, ToJSON gs, FromJSON gs) =>
  ((a -> Texture) -> EngineState -> gs -> Event -> IO gs) ->
  ((a -> Texture) -> EngineState -> gs -> [Sprite]) ->
  Dim ->
  Double ->
  Maybe LiveCodeState ->
  (a -> IO DynamicImage) ->
  [a] ->
  gs ->
  IO EngineConfig
wireEngineConfig stepGameState visualize ecDim ecScale liveCodeState loadTx allTextures initialGameState = do
  recoveredGameState <- for liveCodeState startLiveCode
  gameStateMVar <- newMVar $ fromMaybe initialGameState $ join recoveredGameState
  texturesMVar <- newMVar mempty
  pure $
    EngineConfig
      { ecStepGameState = \es event -> do
          modifyMVar_ gameStateMVar $ \old_gs -> do
            textures <- readTextures texturesMVar
            new_gs <- stepGameState textures es old_gs event
            for_ liveCodeState $ flip liveCodeSwitch new_gs
            pure new_gs,
        ecVisualize = \es -> do
          textures <- readTextures texturesMVar
          visualize textures es <$> readMVar gameStateMVar,
        ecDim = ecDim,
        ecScale = ecScale,
        ecCheckIfContinue = pure . not . gameExitRequested,
        ecGameDebugInfo = \EngineState {..} -> debugPrint <$> readMVar gameStateMVar
      }
  where
    readTextures texturesMVar = do
      textures' <- readMVar texturesMVar
      textures <-
        if null textures' then do
          textures'' <- loadTextures
          void $ swapMVar texturesMVar textures''
          pure textures''
        else
          pure textures'
      pure $ \t ->
        fromMaybe (error $ "error loading texture " <> show t <> ", did you forget putting it into all_textures?") $
          mapLookup t textures
    loadTextures = do
      lt' <- for allTextures $ \i -> (i,) <$> (either fail pure =<< runExceptT . loadTexture =<< loadTx i)
      pure $ foldl (\m (k, v) -> mapInsert k v m) mempty lt'

-- NOTE: resurrect this when implementing dynamically loaded textures
-- updateTextureCache loadedTexturesMVar visualizations loadTx
-- updateTextureCache :: Ord a => MVar (Map a Texture) -> [Sprite] -> (a -> IO DynamicImage) -> IO ()
-- updateTextureCache loadedTexturesMVar visualizations f' =
--   modifyMVar loadedTexturesMVar $ \loadedTextures -> do
--     let f (acc, loadedTextures') = \(Sprite area t) -> case t of
--           DynamicSprite ref ->
--             case mapLookup ref loadedTextures' of
--               Nothing -> do
--                 texture <- either fail pure =<< (runExceptT . loadTexture) =<< f' ref
--                 pure (TexturePlacements texture area : acc, mapInsert ref texture loadedTextures')
--               Just texture -> pure (TexturePlacements texture area : acc, loadedTextures')
--           s@Rectangle{} -> pure (s : acc, loadedTextures')
--     swap <$> foldlM f ([], loadedTextures) visualizations
