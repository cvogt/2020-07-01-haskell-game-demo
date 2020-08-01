module Platformer.GameState where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.List (zip)
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime

data TextureId = FloorPlate | MainCharacter
  deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

textures :: TextureId -> (Scale, FilePath)
textures = \case
  MainCharacter -> (1, "main_character.png")
  FloorPlate -> (1, "floor_plate.png")

newtype Board = Board {unBoard :: Map Pos TextureId} deriving newtype (Show, Semigroup, Monoid, NFData)

data GameState = GameState
  { gsCollisions :: Corners (Maybe Area),
    gsVelocityY :: Double,
    gsVelocityX :: Double,
    gsMainCharacter :: Pos,
    gsMainCharacterPrevious :: Pos,
    gsPenetrable :: Board,
    gsRoom :: Board
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

gridsize :: Num n => n
gridsize = 12

makeInitialGameState :: Dim -> GameState
makeInitialGameState dim =
  GameState
    { gsCollisions = Corners Nothing Nothing Nothing Nothing,
      gsVelocityY = 0,
      gsVelocityX = 0,
      gsMainCharacter = dim * (0.5, 0),
      gsMainCharacterPrevious = dim * (0.5, 0),
      gsPenetrable = Board $ mempty,
      gsRoom =
        Board
          $ mapInsert (240, 188) FloorPlate
          $ mapInsert (240, 176) FloorPlate
          $ mapFromList
          $ concat
          $ take 10
          $ (iterate (+ 12) 200 <&>)
          $ (\r -> take 60 $ (iterate (+ 12) 0 `zip` repeat r) `zip` (repeat FloorPlate))
    }

stepGameStatePure :: (TextureId -> Dim) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure area gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'Space KeyState'Pressed -> gs {gsVelocityY = -220}
  RenderEvent _ ->
    let speedX = 100
        newMainCharacter =
          move
            esTimePassed
            (area MainCharacter, gsMainCharacter)
            gsMainCharacterPrevious
            gsVelocityX
            gsVelocityY
            $ (area FloorPlate,) <$> (keys $ unBoard gsRoom)
     in gs
          { gsMainCharacter = newMainCharacter,
            gsMainCharacterPrevious = gsMainCharacter,
            gsVelocityY =
              if gsVelocityY /= 0 && snd gsMainCharacter == snd newMainCharacter
                then 0
                else gsVelocityY + 9.81 * esTimePassed * 55,
            gsVelocityX =
              if Key'A `setMember` esKeysPressed
                then - speedX
                else
                  if Key'D `setMember` esKeysPressed
                    then speedX
                    else 0
          }
  _ -> gs

instance FromJSON Board where parseJSON = fmap (Board . mapFromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . mapToList . unBoard
