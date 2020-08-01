module Playtime.Event
  ( module Playtime.Event,
    Key (..),
    KeyState (..),
    MouseButton (..),
    MouseButtonState (..),
  )
where

import "GLFW-b" Graphics.UI.GLFW (Key (..), KeyState (..), MouseButton (..), MouseButtonState (..))
import My.Prelude
import Playtime.Geometry

data Event
  = RenderEvent SystemTime
  | MouseEvent MouseButton MouseButtonState
  | KeyEvent Key KeyState
  | CursorPosEvent Pos
  | WindowSizeEvent Int Int
  | WindowCloseEvent
  deriving (Show)
