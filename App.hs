module App where

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Control.Concurrent.STM
import Control.Monad.Trans.RWS

import Events
import Object
import Paddle

data AppEnv = AppEnv
  { envWindow :: !GLFW.Window
  , envPaddle :: !Object
  , envQueue :: TQueue Event
  }

data AppState = AppState
  { stateLeftPaddle :: Paddle
  , stateRightPaddle :: Paddle
  }


type App = RWST AppEnv () AppState IO ()
