module Playtime.UI where

import Data.Aeson (FromJSON, ToJSON)
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime.EngineState
import Playtime.Event
import Playtime.Geometry

data DragAndDrop = DragAndDrop Pos Dim deriving (Show, Generic, NFData, ToJSON, FromJSON)

dragAndDrop ::
  EngineState ->
  gs ->
  MouseButton ->
  [Area] ->
  ([Pos] -> gs -> gs) ->
  Maybe DragAndDrop ->
  (Maybe DragAndDrop -> gs -> gs) ->
  Event ->
  gs
dragAndDrop EngineState {..} gs mb areas setPoss dragAndDrop' setDragAndDrop =
  let poss = snd <$> areas
   in \case
        MouseEvent mb' MouseButtonState'Pressed
          | mb == mb' ->
            let clicked = find (isWithin esCursorPos) areas
             in gs
                  & (setDragAndDrop $ clicked <&> \(_, pos) -> DragAndDrop pos $ pos - esCursorPos)
                  & (setPoss $ poss \\ catMaybes [snd <$> clicked])
        MouseEvent mb' MouseButtonState'Released
          | mb == mb' ->
            gs
              & (setDragAndDrop Nothing)
              & (setPoss $ catMaybes [dragAndDrop' <&> \(DragAndDrop pos _) -> pos] <> poss)
        CursorPosEvent cursor ->
          gs
            & (setDragAndDrop $ dragAndDrop' <&> (\(DragAndDrop _ offset) -> DragAndDrop (cursor + offset) offset))
        _ -> gs

showDragAndDrop :: Maybe DragAndDrop -> (Pos -> a) -> [a]
showDragAndDrop dragAndDrop' sprite' = catMaybes [dragAndDrop' <&> (\(DragAndDrop pos _) -> sprite' pos)]

deleteOnClick :: EngineState -> gs -> MouseButton -> [Area] -> ([Pos] -> gs -> gs) -> Event -> gs
deleteOnClick EngineState {..} gs mb areas setPoss =
  let poss = snd <$> areas
   in \case
        MouseEvent mb' MouseButtonState'Pressed
          | mb == mb' ->
            let clicked = find (isWithin esCursorPos) areas
             in gs & (setPoss $ poss \\ catMaybes [snd <$> clicked])
        _ -> gs
