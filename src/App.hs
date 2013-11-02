module App where

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Control.Concurrent.STM
import Control.Monad.Trans.RWS

import Events
import Graphics.Object
import Objects.Paddle
import Objects.Ball

data AppEnv = AppEnv
  { envWindow :: !GLFW.Window
  , envPaddle :: !Object
  , envBall :: !Object
  , envQueue :: TQueue Event
  }

data AppState = AppState
  { stateLeftPaddle :: Paddle
  , stateRightPaddle :: Paddle
  , stateBall :: Ball
  }


type App = RWST AppEnv () AppState IO ()
