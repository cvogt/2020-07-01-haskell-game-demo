module Playtime.GL where

import Codec.Picture (DynamicImage (ImageRGBA8), Image (Image))
import Data.Vector.Storable (unsafeWith)
import GHC.Err (error)
import GHC.Float (double2Float, int2Double, int2Float)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU.Errors as GLU
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.IO
import My.Prelude
import Playtime.Geometry
import Playtime.Texture

renderGL :: GLFW.Window -> Dim -> [Sprite] -> IO ()
renderGL window (w, h) sprites = do
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 w h 0 0 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  GL.matrixMode $= GL.Modelview 0
  -- GL.lineSmooth $= GL.Disabled

  -- enable png alpha channel transparancy
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  checkErrorsGLU "before"

  for_ (reverse sprites) $ \(Rectangle area tpe) -> case tpe of
    Right (fillType, (RGBA r g b a)) -> do
      GL.texture GL.Texture2D $= GL.Disabled
      GL.currentColor $= GL.Color4 (int2Float r / 255) (int2Float g / 255) (int2Float b / 255) (int2Float a / 255)
      mode <- case fillType of
        Solid -> pure GL.Quads
        Border l -> do
          GL.lineWidth $= l
          pure GL.Lines
      GL.renderPrimitive mode $ do
        let Corners c1 c2 c3 c4 = corners area
        forM_ [c1, c2, c2, c3, c3, c4, c4, c1] vertex
    Left (Texture _ glObject _) -> do
      -- if abs ((width dim / height dim) - (width dim' / height dim')) > 0.001
      --   then error $ show (dim, dim')
      --   else pure ()
      GL.currentColor $= GL.Color4 @Float 255 255 255 1
      GL.texture GL.Texture2D $= GL.Enabled
      GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
      GL.textureBinding GL.Texture2D $= Just glObject
      GL.renderPrimitive GL.Quads $ do
        let Corners s1 s2 s3 s4 = cornerScales
            Corners c1 c2 c3 c4 = corners area
        forM_ [(s1, c1), (s2, c2), (s3, c3), (s4, c4)] $ \(s, c) -> do
          texCoord s
          vertex c

  checkErrorsGLU "after"
  GLFW.swapBuffers window
  where
    texCoord (sx, sy) = GL.texCoord $ GL.TexCoord2 (double2Float sx) (double2Float sy) -- remember 1 makes this match the size of the vertex/quad
    vertex (x, y) = GL.vertex $ GL.Vertex2 (double2Float x) (double2Float y)
    checkErrorsGLU csg = void $ error . ("GLU.errors " <>) . (csg <>) . (": " <>) . show <$> GL.get GLU.errors

-- loadTextureId :: FilePath -> ExceptT [Char] IO Texture
-- loadTextureId file = ExceptT (first ("loadTextureId: " <>) <$> readPng file) >>= loadTexture

loadTexture :: DynamicImage -> ExceptT [Char] IO Texture
loadTexture img = ExceptT $ case img of
  ImageRGBA8 img'@(Image width height dat) -> unsafeWith dat $ \ptr -> do
    let txSize = GL.TextureSize2D (fromIntegral width) (fromIntegral height)
    [texture] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D $= Just texture
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 txSize 0 $ GL.PixelData GL.RGBA GL.UnsignedByte ptr
    pure $ Right $ Texture (int2Double width, int2Double height) texture img'
  _ -> pure $ Left $ "loadTexture error: We currently only support png graphic files JuicyPixles reads as ImageRGBA8."
