module Objects.Ball
(Ball, makeBall, ballUpdate, ballPosition)
where

import Graphics.Rendering.OpenGL (GLfloat, Vertex2(Vertex2))
import System.Random
import Prelude hiding (Left, Right)

import Objects.Paddle hiding (Up, Down)

data HorizDirection = Left | Right
  deriving (Show, Eq)

data VertDirection = Up | Down
  deriving (Show, Eq)

data Ball = Ball
  { position :: Vertex2 GLfloat
  , vertDirection :: VertDirection
  , horizDirection :: HorizDirection
  , randomGen :: StdGen
  }
  deriving Show

instance Num a => Num (Vertex2 a) where
  Vertex2 x1 y1 + Vertex2 x2 y2 = Vertex2 (x1+x2) (y1+y2)
  Vertex2 x1 y1 - Vertex2 x2 y2 = Vertex2 (x1-x2) (y1-y2)
  Vertex2 x1 y1 * Vertex2 x2 y2 = Vertex2 (x1*x2) (y1*y2)
  abs (Vertex2 x y) = Vertex2 (abs x) (abs y)
  signum (Vertex2 x y) = Vertex2 (signum x) (signum y)
  fromInteger i = Vertex2 (fromInteger i) (fromInteger i)

makeBall :: IO Ball
makeBall = do
  gen <- newStdGen
  return $ randomDirection $ Ball (Vertex2 0.0 0.0) Up Left gen

randomDirection :: Ball -> Ball
randomDirection ball =
  ball { vertDirection = vert
       , horizDirection = horiz
       , randomGen = newRandomGen
       }
  where
    (rand, newRandomGen) = randomR (0,3) (randomGen ball)
    (vert, horiz) = chooseDirection rand
    chooseDirection :: Int -> (VertDirection, HorizDirection)
    chooseDirection x
      | x == 0 = (Up, Left)
      | x == 1 = (Up, Right)
      | x == 2 = (Down, Left)
      | x == 3 = (Down, Right)

directionToVector :: VertDirection -> HorizDirection -> Vertex2 GLfloat
directionToVector Up Left = Vertex2 (-0.01) 0.01
directionToVector Up Right = Vertex2 0.01 0.01
directionToVector Down Left = Vertex2 (-0.01) (-0.01)
directionToVector Down Right = Vertex2 0.01 (-0.01)

ballUpdate :: Ball -> Paddle -> Paddle -> (Ball, Bool)
ballUpdate ball@(Ball pos@(Vertex2 x y) vert horiz rand) leftPaddle rightPaddle
  | x < -1.0 = (randomDirection ball { position = Vertex2 0.0 0.0 }, False)
  | x > 1.0 = (randomDirection ball { position = Vertex2 0.0 0.0 }, False)
  | y > 0.95 && vert == Up = (ball { vertDirection = Down }, True)
  | y < -0.95 && vert == Down = (ball { vertDirection = Up }, True)
  | x < -0.82 && x > -0.85 && abs (ly - y) < 0.25 && horiz == Left
      = (ball { horizDirection = Right }, True)
  | x > 0.82 && x < 0.85 && abs (ry - y) < 0.25 && horiz == Right
      = (ball { horizDirection = Left }, True)
  | otherwise = (ball { position = pos + directionToVector vert horiz }, False)
  where
    Vertex2 lx ly = paddlePosition leftPaddle
    Vertex2 rx ry = paddlePosition rightPaddle

ballPosition = position
