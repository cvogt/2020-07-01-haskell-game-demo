module SpaceMiner.GameState where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import GHC.Float (int2Double)
import GHC.Real (floor)
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime

data TextureId = Inventory | RedResource | TopWall | MainCharacter | FloorPlate
  deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

textures :: TextureId -> (Scale, FilePath)
textures = \case
  Inventory -> (1, "inventory.png")
  RedResource -> (1, "red_resource.png")
  TopWall -> (1, "top_wall.png")
  MainCharacter -> (1, "main_character.png")
  FloorPlate -> (1, "floor_plate.png")

newtype Board = Board {unBoard :: Map Pos TextureId} deriving newtype (Show, Semigroup, Monoid, NFData)

data UIMode = TexturePlacementMode TextureId | TextureMoveMode deriving (Show, Generic, NFData, ToJSON, FromJSON)

data GameState = GameState
  { gsUIMode :: UIMode,
    gsCollisions :: (Maybe Area, Maybe Area, Maybe Area, Maybe Area),
    gsCandidates :: [Pos],
    gsFloor :: Board,
    gsRoom :: Board,
    gsLastPlacement :: Pos,
    gsMainCharacter :: Pos,
    gsMainCharacterPrevious :: Pos
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

gridsize :: Num n => n
gridsize = 12

makeInitialGameState :: Dim -> GameState
makeInitialGameState dim =
  GameState
    { gsUIMode = TexturePlacementMode FloorPlate,
      gsCandidates = mempty,
      gsCollisions = (Nothing, Nothing, Nothing, Nothing),
      gsFloor = mempty,
      gsRoom = mempty,
      gsLastPlacement = 0,
      gsMainCharacter = dim / (2 :: Scale),
      gsMainCharacterPrevious = dim / (2 :: Scale)
    }

stepGameStatePure :: (TextureId -> Dim) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure tDim gs@GameState {..} EngineState {..} = \case
  CursorPosEvent _ ->
    let placement = bimap (gridify) (gridify) esCursorPos
        gridify = (* gridsize) . int2Double . floor . (/ gridsize)
     in gs
          { gsLastPlacement = placement,
            gsFloor =
              case gsUIMode of
                TexturePlacementMode texture ->
                  case (`setMember` esMousePressed) of
                    f | f MouseButton'1 && texture == FloorPlate -> Board $ mapInsert placement texture (unBoard gsFloor)
                    f | f MouseButton'2 -> Board $ mapDelete placement (unBoard gsFloor)
                    _ -> gsFloor
                TextureMoveMode -> gsFloor,
            gsRoom =
              case gsUIMode of
                TexturePlacementMode texture ->
                  case (`setMember` esMousePressed) of
                    f | f MouseButton'1 && texture /= FloorPlate -> Board $ mapInsert placement texture (unBoard gsRoom)
                    f | f MouseButton'2 -> Board $ mapDelete placement (unBoard gsRoom)
                    _ -> gsRoom
                TextureMoveMode -> gsRoom
          }
  KeyEvent key KeyState'Pressed ->
    gs
      { gsUIMode = case key of
          Key'1 -> TexturePlacementMode FloorPlate
          Key'2 -> TexturePlacementMode TopWall
          Key'3 -> TextureMoveMode
          _ -> gsUIMode
      }
  RenderEvent _ ->
    if OneTimeEffect Reset `setMember` esActions
      then gs {gsFloor = mempty, gsRoom = mempty}
      else
        let distancePerSec = 100
            velocityX = if MovementAction Left' `setMember` esActions then 0 - distancePerSec else if MovementAction Right' `setMember` esActions then 0 + distancePerSec else 0
            velocityY = if MovementAction Up `setMember` esActions then 0 - distancePerSec else if MovementAction Down `setMember` esActions then 0 + distancePerSec else 0
         in gs
              { gsMainCharacter = move esTimePassed (tDim MainCharacter, gsMainCharacter) gsMainCharacterPrevious velocityX velocityY $ (tDim FloorPlate,) <$> (keys $ unBoard gsRoom),
                gsMainCharacterPrevious = gsMainCharacter
              }
  _ -> gs

instance FromJSON Board where parseJSON = fmap (Board . mapFromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . mapToList . unBoard
