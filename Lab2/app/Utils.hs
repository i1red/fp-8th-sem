module Utils where

import Data.List.Split (splitPlacesBlanks)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)

segmentRange :: Double -> Double -> Int -> [(Double, Double)]
segmentRange start end count =
  map nthSegment [1 .. count]
  where
    segmentLen = (end - start) / fromIntegral count
    nthSegment n = (\k -> (start + (k - 1) * segmentLen, start + k * segmentLen)) $ fromIntegral n

getChunkSizes :: Int -> Int -> [Int]
getChunkSizes chunkCount listLength =
  replicate enlargedChunkCount (baseChunkSize + 1) ++ replicate (chunkCount - enlargedChunkCount) baseChunkSize
  where
    baseChunkSize = listLength `div` chunkCount
    enlargedChunkCount = listLength `rem` chunkCount

timeIt :: a -> IO (NominalDiffTime, a)
timeIt calculation = do
  t0 <- getCurrentTime
  let result = calculation
  t1 <- result `seq` getCurrentTime
  return (diffUTCTime t1 t0, result)
