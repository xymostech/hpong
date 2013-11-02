module Sound.Wave where

import qualified Data.ByteString as B
import System.IO
import System.FilePath ((</>))
import Data.Word
import Data.List
import Data.List.Split
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad
import Data.Functor

data WaveFile = WaveFile
  { handle :: Handle
  , numChannels :: Int
  , sampleRate :: Int
  , sampleSize :: Int
  , readStart :: Int
  , samplesRead :: Int
  , totalSamples :: Int
  } deriving Show

checkRiffHeader :: B.ByteString -> Bool
checkRiffHeader header =
  riff == [82, 73, 70, 70] && format == [87, 65, 86, 69]
  where
    riff = B.unpack $ B.take 4 header
    format = B.unpack . B.take 4 $ B.drop 8 header

readLittleEndian :: [Word8] -> Int
readLittleEndian = foldr (\num prod -> 256 * prod + fromIntegral num) 0

readFmtSubChunk :: Handle -> MaybeT IO B.ByteString
readFmtSubChunk handle = do
  header <- liftIO $ B.hGet handle 8
  guard $ (B.unpack $ B.take 4 header) == [0x66, 0x6d, 0x74, 0x20]
  let sizeBytes = B.unpack . B.take 4 $ B.drop 4 header
  let size = readLittleEndian sizeBytes
  chunkData <- liftIO $ B.hGet handle size
  return chunkData

splitByteString :: B.ByteString -> [Int] -> [B.ByteString]
splitByteString string [] = [string]
splitByteString string (x:xs) = (B.take x string) : splitByteString (B.drop x string) xs

makeWaveFile :: Handle -> B.ByteString -> WaveFile
makeWaveFile handle formatChunk =
  WaveFile handle channels sampleRate (sampleSize `div` 8)
           (riffChunkSize + fmtChunkSize) 0 0
  where
    formatData:channelData:sampleRateData:_:_:sampleSizeData:_ =
      splitByteString formatChunk [2, 2, 4, 4, 2, 2]
    1 = readLittleEndian $ B.unpack formatData
    channels = readLittleEndian $ B.unpack channelData
    sampleRate = readLittleEndian $ B.unpack sampleRateData
    sampleSize = readLittleEndian $ B.unpack sampleSizeData
    riffChunkSize = 12
    fmtChunkSize = B.length formatChunk + 8

setupWaveFile :: WaveFile -> MaybeT IO WaveFile
setupWaveFile file = do
  header <- liftIO $ B.hGet (handle file) 8
  guard $ (B.unpack $ B.take 4 header) == [0x64, 0x61, 0x74, 0x61]
  let chunkSize = readLittleEndian $ B.unpack $ B.take 4 $ B.drop 4 header
  return file
    { totalSamples = chunkSize `div` (numChannels file) `div` (sampleSize file)
    , readStart = (readStart file) + 8
    }

readSamples :: WaveFile -> Int -> IO (WaveFile, [Int])
readSamples file num = do
  sampleData <- B.hGet (handle file) dataSize
  let samples = map readLittleEndian $ chunksOf (sampleSize file) $ B.unpack sampleData
  return (file { samplesRead = (samplesRead file) + toRead }, samples)
    where
      toRead = min num (totalSamples file - samplesRead file)
      dataSize = toRead * (numChannels file) * (sampleSize file)

rewindWaveFile :: WaveFile -> IO WaveFile
rewindWaveFile file = do
  hSeek (handle file) AbsoluteSeek (fromIntegral $ readStart file)
  return file { samplesRead = 0 }

createWaveFile :: FilePath -> MaybeT IO WaveFile
createWaveFile path = do
  file <- liftIO $ openFile path ReadMode
  headerCorrect <- liftIO $ checkRiffHeader <$> B.hGet file 12
  guard headerCorrect
  subChunk <- readFmtSubChunk file
  let waveFile = makeWaveFile file subChunk
  setupWaveFile waveFile

deleteWaveFile :: WaveFile -> IO ()
deleteWaveFile file = do
  hClose (handle file)
