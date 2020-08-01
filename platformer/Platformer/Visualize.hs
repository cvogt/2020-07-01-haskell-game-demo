module Platformer.Visualize where

import qualified Data.Map as Map
import My.Prelude
import Platformer.GameState
import Playtime

visualize :: (TextureId -> Pos -> Sprite) -> EngineState -> GameState -> [Sprite]
visualize sprite EngineState {..} GameState {..} =
  [sprite MainCharacter gsMainCharacter] <> room
  where
    room = (Map.toList $ unBoard gsRoom) <&> \(pos, t) -> sprite t pos

-- backup of grouping logic as reminder if needed: (groupWith snd $ Map.toList $ unBoard gsFloor) <&> \ne@((_, t) :| _) ->
