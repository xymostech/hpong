module Sounds where

import Sound.OpenAL
import Control.Concurrent
import qualified Data.Vector.Storable as V
import Data.Int
import Foreign.Ptr
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Concurrent.MVar

import Wave

setupSound :: IO ()
setupSound = do
  Just device <- openDevice Nothing
  Just context <- createContext device []
  currentContext $= Just context

cleanupSound :: IO ()
cleanupSound = do
  Just context <- get currentContext
  Just device <- get $ contextsDevice context
  closeDevice device
  return ()

makeBufferData :: WaveFile -> Int -> Ptr a -> BufferData a
makeBufferData file length ptr =
  BufferData (MemoryRegion ptr (2 * fromIntegral length))
    Mono16
    (fromIntegral (sampleRate file))

convertSamples :: [Int] -> [Int16]
convertSamples samples =
  map fromIntegral samples

readSamplesToBuffer :: WaveFile -> Int -> Buffer -> IO WaveFile
readSamplesToBuffer file num buffer = do
  (newFile, samples) <- readSamples file num
  let vec = (V.fromList $ convertSamples samples)
  V.unsafeWith vec $ \ptr ->
    bufferData buffer $= makeBufferData newFile (V.length vec) ptr
  return newFile

test :: FilePath -> IO ()
test file = do
  (source, buffer) <- getStuff
  sourceGain source $= 0.05
  Just waveFile <- runMaybeT $ createWaveFile file
  waveFile <- readSamplesToBuffer waveFile (totalSamples waveFile) buffer
  queueBuffers source [buffer]
  play [source]

waitPlayThread :: WaveFile -> Source -> Buffer -> MVar Bool -> IO ()
waitPlayThread file source buf stop = do
  threadDelay 10000
  state <- get $ sourceState source
  stopped <- tryTakeMVar stop
  if state == Playing || stopped == Nothing
    then waitPlayThread file source buf stop
    else cleanupPlayThread file source buf

cleanupPlayThread :: WaveFile -> Source -> Buffer -> IO ()
cleanupPlayThread file source buf = do
  deleteObjectNames [source]
  deleteObjectNames [buf]
  deleteWaveFile file

playFileThread :: FilePath -> MVar Bool -> IO ()
playFileThread file stop = do
  (source, buf) <- getStuff
  sourceGain source $= 0.1
  Just waveFile <- runMaybeT $ createWaveFile file
  waveFile <- readSamplesToBuffer waveFile (totalSamples waveFile) buf
  buffer source $= Just buf
  play [source]
  waitPlayThread waveFile source buf stop

playFile :: FilePath -> IO (MVar Bool)
playFile file = do
  stop <- newEmptyMVar
  forkIO $ playFileThread file stop
  return stop

getStuff :: IO (Source, Buffer)
getStuff = do
  [source] <- genObjectNames 1
  [buffer] <- genObjectNames 1
  return (source, buffer)
