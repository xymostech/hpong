import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad (when, unless, guard)
import Data.ByteString as B (readFile, ByteString)
import System.FilePath
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS
import Control.Monad.IO.Class
import Data.Word
import Data.Int
import GHC.Float
import Foreign.C.Types
import Control.Concurrent.STM

import App
import Events
import Shaders
import Buffers
import ArrayObjects
import Object
import Renderable
import Sounds

toCInt :: Int -> CInt
toCInt int = let cint = (fromIntegral int) :: Int32 in CInt cint

sizeCallback :: Window -> Int -> Int -> IO ()
sizeCallback window x y = do
  viewport $= (Position 0 0, Size (toCInt x) (toCInt y))

makePaddle :: IO Object
makePaddle = do
  Just object <- runMaybeT $ makeObject [
     (VertexShader, "tut1.vert.shader"),
     (FragmentShader, "tut1.frag.shader")
    ] vertexPositions indices
    [("position", VertexArrayDescriptor 4 Float 0 $ ptrOffset 0)]
    Triangles 6
  return object
  where
    vertexPositions = [
       0.2, 0.2, 0.0, 1.0,
       0.2, -0.2, 0.0, 1.0,
       -0.2, 0.2, 0.0, 1.0,
       -0.2, -0.2, 0.0, 1.0
      ]
    indices = [
       0, 1, 2,
       2, 1, 3
      ]

setup :: Window -> IO ()
setup win = do
  setupSound
  setWindowSizeCallback win (Just sizeCallback)

  queue <- newTQueueIO

  setupEvents win queue

  paddle <- makePaddle

  let env = AppEnv
        { envWindow = win
        , envObject = paddle
        , envQueue = queue
        }

  let state = AppState
        { statePosition = Vertex2 0.0 0.0
        }

  runRWST run env state

  cleanupSound
  return ()

draw :: App
draw = do
  liftIO $ clearColor $= Color4 0.1 0.1 0.1 0.0
  liftIO $ clear [ColorBuffer]

  object <- asks envObject
  position <- gets statePosition

  liftIO $ renderWith object $ \program -> do
    offsetLoc <- GL.get $ uniformLocation program "offset"
    uniform offsetLoc $= position

getPositionForTime :: Double -> Vertex2 GLfloat
getPositionForTime time = Vertex2 (CFloat $ double2Float $ sin time) 0.0

update :: App
update = do
  Just time <- liftIO $ getTime

  modify $ \s -> s
    { statePosition = getPositionForTime time
    }

handleEvents :: App
handleEvents = do
  queue <- asks envQueue
  e <- liftIO $ atomically $ tryReadTQueue queue
  case e of
    Just (EventKeyPress key modifiers) -> do
      handleEvents
    Just (EventKeyRelease key modifiers) -> do
      handleEvents
    Nothing -> return ()

run :: App
run = do
  win <- asks envWindow

  handleEvents

  update

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
