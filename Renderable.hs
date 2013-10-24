module Renderable where

import Graphics.Rendering.OpenGL

class Renderable a where
  render :: a -> IO ()
  renderWith :: a -> (Program -> IO ()) -> IO ()
