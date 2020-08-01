module ShootEmUp.GameState where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (zip)
import GHC.Float (double2Int, int2Double)
import GHC.Real (mod)
import "GLFW-b" Graphics.UI.GLFW
import My.IO
import My.Prelude
import Playtime
import System.Random

data GameState = GameState
  { gsMainCharacter :: Pos,
    gsEnemies :: [Pos],
    gsStars :: [(Double, Pos)],
    gsHearts :: [Pos],
    gsMaxStarSize :: Double,
    gsDragAndDrop :: Maybe DragAndDrop
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

numEnemies :: Int
numEnemies = 10

data TextureId = Enemy | Heart | Spaceship
  deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

textures :: TextureId -> (Scale, FilePath)
textures = \case
  Spaceship -> (1, "plane.png")
  Enemy -> (0.1, "enemy_red.png")
  Heart -> (0.025, "haskell_love_logo.png")

makeInitialGameState :: Dim -> IO GameState
makeInitialGameState dim = do
  let maxStarSize = 3
  starX <- fmap (fmap int2Double) $ sequence $ replicate 510 $ randomRIO (0, double2Int $ maxStarSize + fst dim)
  starY <- fmap (fmap int2Double) $ sequence $ replicate 510 $ randomRIO (0, double2Int $ maxStarSize + snd dim)
  starSize <- fmap (fmap int2Double) $ sequence $ replicate 510 $ randomRIO (0, double2Int maxStarSize)
  pure
    GameState
      { gsMainCharacter = (10, 200),
        gsEnemies = mempty,
        gsStars = starSize `zip` (starX `zip` starY),
        gsHearts = mempty,
        gsMaxStarSize = maxStarSize,
        gsDragAndDrop = Nothing
      }

stepGameStatePure :: [Int] -> (TextureId -> Dim) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure pre area old_gs es event =
  foldl
    (&)
    old_gs
    [ \gs -> dragAndDrop es gs MouseButton'1 (getBulletAreas gs) setBullets (getDragAndDrop gs) setDragAndDrop event,
      \gs -> deleteOnClick es gs MouseButton'2 (getBulletAreas gs) setBullets event,
      \gs -> stepGameStatePure' pre area gs es event
    ]
  where
    setBullets bullets gs = gs {gsHearts = bullets}
    getBulletAreas gs = (area Heart,) <$> gsHearts gs
    getDragAndDrop gs = gsDragAndDrop gs
    setDragAndDrop v gs = gs {gsDragAndDrop = v}

stepGameStatePure' :: [Int] -> (TextureId -> Dim) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure' randInts tDim gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    gs
      { gsHearts =
          gsHearts
            <> ((gsMainCharacter +) <$> [(300, 100) :: Dim, (300, 145), (325, 200), (325, 290), (300, 340), (300, 390)])
      }
  RenderEvent _ ->
    let distancePerSec = 200
        (_, height) = esDimensions
        direction :: Dim
        direction =
          ( if
                | MovementAction Left' `setMember` esActions -> -1
                | MovementAction Right' `setMember` esActions -> 1
                | True -> 0,
            if
                | MovementAction Up `setMember` esActions -> -1
                | MovementAction Down `setMember` esActions -> 1
                | True -> 0
          )
        bulletVelocity = (300, 0)
        bulletStep :: Dim
        bulletStep = dupe esTimePassed * bulletVelocity
        survivingEnemies =
          flip filter gsEnemies $ \enemyPos ->
            (fst enemyPos > - fst (tDim Enemy) &&)
              $ not
              $ any ((tDim Enemy, enemyPos) `collidesWith`) (bulletTrajectory =<< gsHearts)
          where
            bulletTrajectory pos = (tDim Heart,) <$> trajectoryPixels pos esTimePassed bulletVelocity
        newEnemies = survivingEnemies <> (newEnemyPos <$> take numAdded randInts)
          where
            numAdded = numEnemies - length survivingEnemies
            newEnemyPos :: Int -> Pos
            newEnemyPos y = (1100, int2Double . flip mod (double2Int $ height - (snd $ tDim Enemy)) $ y)
        stepStar :: (Double, Pos) -> (Double, Pos)
        stepStar (size, pos) = (size,) $ modu $ move' pos
          where
            move' = subtract (esTimePassed * size * 6, 0)
            modu = (`mod2` (esDimensions + dupe gsMaxStarSize))
     in gs
          { gsMainCharacter = gsMainCharacter + (dupe $ esTimePassed * distancePerSec) * direction,
            gsEnemies = (subtract $ (esTimePassed * 100, 0)) <$> newEnemies,
            gsStars = stepStar <$> gsStars,
            gsHearts = filter ((< 1024) . fst) gsHearts <&> (+ bulletStep)
          }
  _ -> gs
