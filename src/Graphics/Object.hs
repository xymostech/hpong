module Graphics.Object
( makeObject
, Object
)
where

import Graphics.Rendering.OpenGL as GL
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Data.Word

import Graphics.Renderable
import Graphics.Shaders
import Graphics.Buffers
import Graphics.ArrayObjects

data Object = Object Program [BufferObject] NumArrayIndices
              PrimitiveMode VertexArrayObject

enableVertexArray :: Program -> (String, VertexArrayDescriptor a) -> IO ()
enableVertexArray program (name, descriptor) = do
  location <- get $ attribLocation program name
  vertexAttribArray location $= Enabled
  vertexAttribPointer location $= (ToFloat, descriptor)

makeObject :: [(ShaderType, FilePath)] -> [Float] ->
              [Word16] -> [(String, VertexArrayDescriptor a)] ->
              PrimitiveMode -> NumArrayIndices ->
              MaybeT IO Object
makeObject programSpec vertices indices attribs mode num = do
  program <- makeProgram programSpec
  vertexBuffer <- liftIO $ makeBufferWithData ArrayBuffer vertices StaticDraw
  indexBuffer <- liftIO $ makeBufferWithData ElementArrayBuffer indices StaticDraw
  vao <- liftIO $ makeArrayObject

  liftIO $ bindVertexArrayObject $= Just vao

  liftIO $ bindBuffer ArrayBuffer $= Just vertexBuffer
  liftIO $ bindBuffer ElementArrayBuffer $= Just indexBuffer

  liftIO $ mapM_ (enableVertexArray program) attribs

  liftIO $ bindVertexArrayObject $= Nothing

  liftIO $ bindBuffer ArrayBuffer $= Nothing
  liftIO $ bindBuffer ElementArrayBuffer $= Nothing

  return $ Object program [vertexBuffer, indexBuffer] num mode vao

instance Renderable Object where
  render object = renderWith object (\_ -> return ())

  renderWith (Object program _ size mode vao) m = do
    bindVertexArrayObject $= Just vao
    currentProgram $= Just program

    m program

    drawElements mode size UnsignedShort (ptrOffset 0)

    bindVertexArrayObject $= Nothing
    currentProgram $= Nothing
