module Main where

import Data.Foldable (for_)
import GHC.Conc (numCapabilities)
import IntegrateParallel (integrateParallel)
import Utils (timeIt)

main :: IO ()
main = do
  putStrLn $ "Available number of workers: " ++ show numCapabilities
  for_ [1 .. numCapabilities] printTimings
  where
    printTimings workerCount = do
      putStrLn $ "workers=" ++ show workerCount
      (time, result) <- timeIt $ integrateParallel f start end epsilon workerCount
      putStrLn $ "\ttime: " ++ show time
      putStrLn $ "\tresult: " ++ show result
      where
        f = (\x -> abs (x / 10) ** 0.125)
        start = -2e4
        end = 2e4
        epsilon = 1e-2
