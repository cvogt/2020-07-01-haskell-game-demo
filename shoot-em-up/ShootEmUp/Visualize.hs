module ShootEmUp.Visualize where

import My.Prelude
import Playtime
import ShootEmUp.GameState

visualize :: (TextureId -> Pos -> Sprite) -> EngineState -> GameState -> [Sprite]
visualize sprite EngineState {..} GameState {..} =
  [sprite Spaceship gsMainCharacter]
    <> (sprite Heart <$> gsHearts)
    <> (sprite Enemy <$> gsEnemies)
    <> stars
    <> showDragAndDrop gsDragAndDrop (sprite Heart)
    <> showDragAndDrop gsDragAndDrop (rectangle' (Border 3) (RGBA 255 0 0 255) . spriteArea . sprite Heart)
  where
    stars =
      gsStars <&> \((+ 1) -> size, pos) ->
        rectangle Solid (RGBA 180 180 180 255) (- size, size) pos -- -size for x so stars extend to the left and can leave the screen smoothly
