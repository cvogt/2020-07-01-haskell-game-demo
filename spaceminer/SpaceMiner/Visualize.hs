module SpaceMiner.Visualize where

import Codec.Picture.Types (PixelRGBA8 (PixelRGBA8), pixelAt)
import qualified Data.Map as Map
import GHC.Float (double2Int)
import My.Prelude
import Playtime
import SpaceMiner.GameState

visualize :: (TextureId -> Pos -> Sprite) -> EngineState -> GameState -> [Sprite]
visualize sprite EngineState {..} GameState {..} =
  highlightMouserOver <> sprites
  where
    sprites =
      inventoryUI
        <> [ sprite MainCharacter gsMainCharacter,
             sprite MainCharacter 0,
             sprite MainCharacter 50,
             rectangle (Border 3) (RGBA 255 0 0 255) 24 90,
             rectangle Solid (RGBA 255 255 0 255) 24 (90, 114)
           ]
        <> room
        <> floor
    highlightMouserOver = case findMouseOver of
      Nothing -> []
      Just (Rectangle area _) -> highlight area
    highlight (dim, pos) = [rectangle (Border 3) (RGBA 0 255 0 255) (dim + 4) (pos -2)]
    findMouseOver =
      flip find sprites $ \case
        Rectangle area@(dim', pos) (Left (Texture dim _ img)) ->
          let p = (esCursorPos - pos) * dim / dim'
              transparentPixel = case pixelAt img (double2Int $ fst p) (double2Int $ snd p) of PixelRGBA8 _ _ _ a -> a == 0
           in esCursorPos `isWithin` area && not transparentPixel
        Rectangle area _ -> esCursorPos `isWithin` area
    floor = (Map.toList $ unBoard gsFloor) <&> \(pos, t) -> sprite t pos
    room = (Map.toList $ unBoard gsRoom) <&> \(pos, t) -> sprite t pos
    -- backup of grouping logic as reminder if needed: (groupWith snd $ Map.toList $ unBoard gsFloor) <&> \ne@((_, t) :| _) ->
    inventoryUI =
      translate (200, 100)
        <$> [ sprite RedResource 18,
              sprite MainCharacter 3,
              sprite Inventory 0
            ]
