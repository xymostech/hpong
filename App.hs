module App where

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Control.Concurrent.STM
import Control.Monad.Trans.RWS

import Events
import Object

data AppEnv = AppEnv
  { envWindow :: !GLFW.Window
  , envObject :: !Object
  , envQueue :: TQueue Event
  }

data AppState = AppState
  { statePosition :: Vertex2 GLfloat
  }

type App = RWST AppEnv () AppState IO ()
