module IntegrateParallel where

import Data.Maybe (isJust, fromJust)
import Control.Concurrent (getChanContents, newChan, writeChan)
import GHC.Conc (forkIO)
import Utils (segmentRange, splitIntoChunks)

integrateParallel :: (Double -> Double) -> Double -> Double -> Double -> Int -> IO Double
integrateParallel f start end epsilon workerCount =
  integrateParallelRecursive initialSegmentCount Nothing
  where
    initialSegmentCount = ceiling $ (end - start) * 10

    integrateParallelRecursive segmentCount prevResult = do
      curResult <- integrateParallelImpl segmentCount
      if isJust prevResult && abs (curResult - fromJust prevResult) <= epsilon
        then return curResult
        else integrateParallelRecursive (segmentCount * 2) (Just curResult)
      where
        integrateParallelImpl segmentCount = do
          resultsChan <- newChan
          mapM_ (\chunk -> forkIO $ integrateSequential chunk resultsChan) segmentChunks
          fmap (sum . take workerCount) (getChanContents resultsChan)
          where
            segmentChunks = splitIntoChunks workerCount (segmentRange start end segmentCount)

            integrateSequential segments resultsChan = writeChan resultsChan (sum $ map integrateSegment segments)
              where
                integrateSegment = \(a, b) -> (f a + f b) / 2 * (b - a)
