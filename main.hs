import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad (when, unless, guard)
import Data.ByteString as B (readFile, ByteString)
import System.FilePath
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Word

import Shaders
import Buffers
import ArrayObjects
import Object
import Renderable

data AppEnv = AppEnv
  { envWindow :: !GLFW.Window
  , envObject :: !Object
  }

type App = ReaderT AppEnv IO ()

setup :: Window -> IO ()
setup win = do
  viewport $= (Position 0 0, Size 1440 880)

  let vertexPositions = [
        0.75, 0.75, 0.0, 1.0,
        0.75, -0.75, 0.0, 1.0,
        -0.75, 0.75, 0.0, 1.0,
        -0.75, -0.75, 0.0, 1.0
        ] :: [Float]

  let indices = [
        0, 1, 2,
        2, 1, 3
        ] :: [Word16]

  Just object <- runMaybeT $ makeObject [
            (VertexShader, "tut1.vert.shader"),
            (FragmentShader, "tut1.frag.shader")
           ] vertexPositions indices
           [("position", VertexArrayDescriptor 4 Float 0 $ ptrOffset 0)]
           Triangles 6

  let env = AppEnv {
        envWindow = win
        , envObject = object
        }

  runReaderT run env

draw :: App
draw = do
  liftIO $ clearColor $= Color4 0.1 0.1 0.1 0.0
  liftIO $ clear [ColorBuffer]

  object <- asks envObject

  liftIO $ render object

run :: App
run = do
  win <- asks envWindow

  draw
  liftIO $ GLFW.swapBuffers win

  liftIO $ GLFW.pollEvents
  q <- liftIO $ GLFW.windowShouldClose win
  unless q $ run

main :: IO ()
main = do
  r <- GLFW.init
  when r $ do
    withWindow 1440 880 "Haskell GL" setup
    GLFW.terminate

withWindow :: Int -> Int -> String -> (Window -> IO ()) -> IO ()
withWindow width height name run = do
  GLFW.windowHint $ WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ WindowHint'ContextVersionMinor 1

  ww <- GLFW.createWindow width height name Nothing Nothing
  case ww of
    Just win -> do
      GLFW.makeContextCurrent ww
      run win
      GLFW.destroyWindow win
    Nothing -> return ()
