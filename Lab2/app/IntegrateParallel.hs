module IntegrateParallel where

import Control.Parallel.Strategies (parMap, rseq)
import Data.Maybe (fromJust, isJust)
import GHC.Conc (getNumCapabilities)
import Utils (getChunkSizes, segmentRange)

integrateParallel :: (Double -> Double) -> Double -> Double -> Double -> Int -> Double
integrateParallel f start end epsilon workerCount =
  integrateParallelRecursive initialSegmentCount Nothing
  where
    -- initialSegmentCount is calculated based on formula:
    -- https://en.wikipedia.org/wiki/Trapezoidal_rule#Error_analysis
    initialSegmentCount = ceiling $ sqrt ((end - start) ** 3 / (12 * epsilon))

    integrateParallelRecursive segmentCount prevResult
      | isPrecise = curResult
      | otherwise = integrateParallelRecursive (segmentCount * 2) (Just curResult)
      where
        curResult = integrateParallelImpl segmentCount
        isPrecise = isJust prevResult && abs (curResult - fromJust prevResult) <= epsilon

        integrateParallelImpl segmentCount =
          sum $ parMap rseq (uncurry integrateSequential) (zip workerChunkSizes workerSubRanges)
          where
            workerSubRanges = segmentRange start end workerCount
            workerChunkSizes = getChunkSizes workerCount segmentCount

            integrateSequential segmentCount (start, end) = 
              sum $ map integrateSegment segments
              where
                integrateSegment = \(a, b) -> (f a + f b) / 2 * (b - a)
                segments = segmentRange start end segmentCount
