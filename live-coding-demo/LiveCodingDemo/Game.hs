{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module LiveCodingDemo.Game where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (zip)
import My.Prelude
import Playtime

data TextureId = Heart | Spaceship | Enemy deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

textures :: TextureId -> (Scale, FilePath)
textures = \case
  Spaceship -> (1, "plane.png")
  Enemy -> (0.1, "enemy_red.png")
  Heart -> (0.025, "haskell_love_logo.png")

data GameState = GameState
  { gsPlayer :: Pos,
    gsStars :: [Pos],
    gsHearts :: [Pos],
    gsEnemies :: [Pos]
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

makeInitialGameState :: Dim -> Int -> GameState
makeInitialGameState dimensions seed =
  let rng = mkStdGen seed
      numStars = 500
      (stars, _) = randomPoss rng numStars dimensions
   in GameState
        { gsPlayer = (10000, 10000),
          gsStars = [], --stars,
          gsHearts = [],
          gsEnemies = []
        }

stepGameStatePure :: Int -> (TextureId -> Dim) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure seed tDim gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    gs
      { gsHearts = gsHearts <> ((gsPlayer +) <$> [(300, 100), (300, 145), (325, 200), (325, 290), (300, 340), (300, 390)])
      }
  RenderEvent _ ->
    let rng = mkStdGen seed
        -- collisionMay e h = if (tDim Enemy, e) `collidesWith` (tDim Heart, h) then Just (e, h) else Nothing
        (hitEnemies, hitHearts) = ([],[]) -- unzip $ catMaybes $ collisionMay <$> gsEnemies <*> gsHearts
        numAddedEnemies = 10 - length gsEnemies
        addedEnemies = [] -- repeat 900 `zip` (fst $ randomsNatDouble rng numAddedEnemies $ snd esWindowSize)
        movePlayerY pos =
          if
              | Key'W `setMember` esKeysPressed -> pos - dupe esTimePassed * (0, 200)
              | Key'S `setMember` esKeysPressed -> pos + dupe esTimePassed * (0, 200)
              | True -> pos
        movePlayerX pos =
          if
              | Key'A `setMember` esKeysPressed -> pos - dupe esTimePassed * (200, 0)
              | Key'D `setMember` esKeysPressed -> pos + dupe esTimePassed * (200, 0)
              | True -> pos
        newHearts = (gsHearts \\ hitHearts)
        newEnemies = ((gsEnemies \\ hitEnemies) <> addedEnemies)
     in gs
          { gsPlayer = gsPlayer, -- movePlayerX $ movePlayerY gsPlayer,
            gsStars = gsStars, -- <&> (`mod2` esDimensions) . (subtract $ dupe esTimePassed * (100,0)),
            gsHearts = newHearts, -- filter ((< fst esDimensions) . fst) $ -- <&> (+ dupe esTimePassed * (200, 0)),
            gsEnemies = newEnemies -- filter ((>0) . fst) $ <&> (subtract $ dupe esTimePassed * (200,0))
          }
  _ -> gs

visualize :: (TextureId -> Pos -> Sprite) -> EngineState -> GameState -> [Sprite]
visualize sprite EngineState {..} GameState {..} =
  let
   in [sprite Spaceship gsPlayer]
        <> (sprite Heart <$> gsHearts)
        <> (sprite Enemy <$> gsEnemies)
        <> (rectangle Solid (RGBA 180 180 180 255) 3 <$> gsStars)
