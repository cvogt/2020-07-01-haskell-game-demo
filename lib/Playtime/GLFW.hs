module Playtime.GLFW
  ( withGLFW,
    setEventCallback,
  )
where

import GHC.Err (error)
import GHC.Float (double2Int)
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.IO
import My.Prelude
import Playtime.Event
import Playtime.Geometry

withGLFW :: Dim -> [Char] -> (GLFW.Window -> IO ()) -> IO ()
withGLFW (width, height) title glCode = do
  GLFW.setErrorCallback $ Just $ \e -> error . ("GLFW:" <>) . (show e <>)
  whenM GLFW.init $ flip finally GLFW.terminate $ do
    Just _mon <- GLFW.getPrimaryMonitor
    let fullscreen = Nothing -- Just mon
    GLFW.createWindow (double2Int width) (double2Int height) title fullscreen Nothing >>= \case
      Just window -> flip finally (GLFW.destroyWindow window) $ do
        GLFW.makeContextCurrent $ Just window
        -- setCursorInputMode win CursorInputMode'Hidden
        glCode window
      Nothing -> error "createWindow returned Nothing"

setEventCallback :: GLFW.Window -> (Event -> IO ()) -> IO ()
setEventCallback window stepStates = do
  GLFW.setMouseButtonCallback window $ Just $ \_ button state _modifiers -> stepStates $ MouseEvent button state
  GLFW.setKeyCallback window $ Just $ \_ key _scancode keyState _modifiers -> stepStates $ KeyEvent key keyState
  GLFW.setWindowSizeCallback window $ Just $ \_ width height -> stepStates $ WindowSizeEvent width height
  GLFW.setCursorPosCallback window $ Just $ \_ x y -> stepStates $ CursorPosEvent (x, y)
  GLFW.setWindowCloseCallback window $ Just $ \_ -> stepStates $ WindowCloseEvent
