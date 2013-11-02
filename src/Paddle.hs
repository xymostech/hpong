module Paddle
( makePaddle, paddlePosition, paddlePressed, paddleUpdate
, Paddle, Direction(Up, Down), EventType(Pressed, Released)
)
where

import Graphics.Rendering.OpenGL (Vertex2(Vertex2), GLfloat)

data Direction = Up | Down | Both | None
  deriving (Eq, Show)

(.+) :: Direction -> Direction -> Direction
(.+) None x = x
(.+) x None = x
(.+) Both _ = Both
(.+) _ Both = Both
(.+) x y
  | x == y = x
(.+) Up Down = Both
(.+) Down Up = Both

(.-) :: Direction -> Direction -> Direction
(.-) x None = x
(.-) None x = None
(.-) x Both = None
(.-) x y
  | x == y = None
(.-) Up Down = Up
(.-) Down Up = Down
(.-) Both Up = Down
(.-) Both Down = Up

data Paddle = Paddle
  { position :: Vertex2 GLfloat
  , direction :: Direction
  }
  deriving Show

makePaddle :: Vertex2 GLfloat -> Paddle
makePaddle position =
  Paddle position None

data EventType = Pressed | Released

paddlePressed :: Paddle -> Direction -> EventType -> Paddle
paddlePressed paddle@(Paddle _ dir) x Pressed = paddle { direction = dir .+ x }
paddlePressed paddle@(Paddle _ dir) x Released = paddle { direction = dir .- x }

paddleUpdate :: Paddle -> Paddle
paddleUpdate paddle@(Paddle (Vertex2 x y) dir)
  | dir == None = paddle
  | dir == Both = paddle
  | dir == Up = paddle { position = Vertex2 x (min (y + 0.01) 0.8) }
  | dir == Down = paddle { position = Vertex2 x (max (y - 0.01) (-0.8)) }

paddlePosition = position
