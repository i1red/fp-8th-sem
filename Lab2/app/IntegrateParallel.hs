module IntegrateParallel where

import Control.Concurrent (forkIO, forkOS, forkOn, getChanContents, newChan, writeChan)
import Data.Foldable (for_)
import Data.Maybe (fromJust, isJust)
import GHC.Conc (getNumCapabilities)
import Utils (getChunkSizes, segmentRange)

integrateParallel :: (Double -> Double) -> Double -> Double -> Double -> Int -> IO Double
integrateParallel f start end epsilon workerCount =
  integrateParallelRecursive initialSegmentCount Nothing
  where
    -- initialSegmentCount is calculated based on formula:
    -- https://en.wikipedia.org/wiki/Trapezoidal_rule#Error_analysis
    initialSegmentCount = ceiling $ sqrt ((end - start) ** 3 / (12 * epsilon))

    integrateParallelRecursive segmentCount prevResult = do
      curResult <- integrateParallelImpl segmentCount
      if isJust prevResult && abs (curResult - fromJust prevResult) <= epsilon
        then return curResult
        else integrateParallelRecursive (segmentCount * 2) (Just curResult)
      where
        integrateParallelImpl segmentCount = do
          resultsChan <- newChan
          mapM_ (\(chunkSize, subRange) -> forkIO $ integrateSequential chunkSize subRange resultsChan) (zip workerChunkSizes workerSubRanges)
          fmap (sum . take workerCount) (getChanContents resultsChan)
          where
            workerSubRanges = segmentRange start end workerCount
            workerChunkSizes = getChunkSizes workerCount segmentCount

            integrateSequential segmentCount (start, end) resultsChan = do
              _ <- result `seq` pure () -- forcing result evaluation, so speed up from using threads is achieved
              writeChan resultsChan result
              where
                integrateSegment = \(a, b) -> (f a + f b) / 2 * (b - a)
                result = sum $ map integrateSegment (segmentRange start end segmentCount)
