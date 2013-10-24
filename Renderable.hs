module Renderable where

class Renderable a where
  render :: a -> IO ()

