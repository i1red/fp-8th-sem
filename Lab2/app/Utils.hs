module Utils where

import Data.List.Split (splitPlacesBlanks)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)

segmentRange :: Double -> Double -> Int -> [(Double, Double)]
segmentRange start end count =
  map nthSegment [1 .. count]
  where
    segmentLen = (end - start) / fromIntegral count
    nthSegment n = (\k -> (start + (k - 1) * segmentLen, start + k * segmentLen)) $ fromIntegral n

splitIntoChunks :: Int -> [a] -> [[a]]
splitIntoChunks chunkCount list = splitPlacesBlanks chunkSizes list
  where
    baseChunkSize = length list `div` chunkCount
    enlargedChunkCount = length list `rem` chunkCount
    chunkSizes = replicate enlargedChunkCount (baseChunkSize + 1) ++ replicate (chunkCount - enlargedChunkCount) baseChunkSize

timeIoAction :: IO a -> IO (NominalDiffTime, a)
timeIoAction calculation = do
  t0 <- getCurrentTime
  result <- calculation
  t1 <- result `seq` getCurrentTime
  return (diffUTCTime t1 t0, result)
