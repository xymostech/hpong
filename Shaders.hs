module Shaders
( loadShader
, makeProgram
) where

import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad (guard, unless)
import Data.ByteString as B (readFile, ByteString)
import System.FilePath
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

makeShader :: ShaderType -> ByteString -> MaybeT IO Shader
makeShader shaderType shaderSource = do
  shader <- lift $ createShader shaderType
  lift $ shaderSourceBS shader $= shaderSource
  lift $ compileShader shader
  status <- lift $ get $ compileStatus shader
  unless status $ do
    infoLog <- lift $ get $ shaderInfoLog shader
    lift $ putStrLn $ "Failed compiling shader:\n" ++ infoLog
    lift $ deleteObjectName shader
  guard status
  return shader

loadShader :: ShaderType -> FilePath -> MaybeT IO Shader
loadShader shaderType filePath = do
  shaderSource <- lift $ B.readFile filePath
  makeShader shaderType shaderSource

loadAndAttachShader :: Program -> (ShaderType, FilePath) -> MaybeT IO ()
loadAndAttachShader program (shaderType, filePath) = do
  shader <- loadShader shaderType filePath
  lift $ attachShader program shader

makeProgram :: [(ShaderType, FilePath)] -> MaybeT IO Program
makeProgram shaderList = do
  program <- lift createProgram
  shaders <- mapM loadShaderTuple shaderList
  lift $ mapM_ (attachShader program) shaders
  lift $ linkProgram program
  lift $ mapM_ (detachShader program) shaders
  lift $ deleteObjectNames shaders
  status <- lift $ get $ linkStatus program
  unless status $ do
    infoLog <- lift $ get $ programInfoLog program
    lift $ putStrLn $ "Failed linking program:\n" ++ infoLog
    lift $ deleteObjectName program
  guard status
  return program
  where
    loadShaderTuple :: (ShaderType, FilePath) -> MaybeT IO Shader
    loadShaderTuple (shaderType, filePath) =
      loadShader shaderType filePath
