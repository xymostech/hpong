module Graphics.Buffers
( makeBufferWithData
, ptrOffset
)
where

import Graphics.Rendering.OpenGL
import Foreign.Storable
import Foreign.Ptr
import Data.Array.Storable

makeBuffer :: IO BufferObject
makeBuffer = genObjectName

withBufferBound :: BufferObject -> BufferTarget -> IO () -> IO ()
withBufferBound buffer target m = do
  bindBuffer target $= Just buffer
  m
  bindBuffer target $= Nothing

setBufferData :: Storable a => BufferObject -> BufferTarget ->
                 [a] -> BufferUsage -> IO ()
setBufferData buffer target elems usage = do
  arr <- newListArray (0, length elems - 1) elems
  withBufferBound buffer target $ do
    withStorableArray arr $ \ptr ->
      bufferData target $= (arrSize elems, ptr, usage)
  where
    arrSize [] = toEnum 0
    arrSize (x:xs) = toEnum $ (length elems) * (sizeOf x)

makeBufferWithData :: Storable a => BufferTarget -> [a] ->
                      BufferUsage -> IO BufferObject
makeBufferWithData target elems usage = do
  buffer <- makeBuffer
  setBufferData buffer target elems usage
  return buffer

ptrOffset x = plusPtr nullPtr x
