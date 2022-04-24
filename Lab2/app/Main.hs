module Main where

import GHC.Conc (numCapabilities)
import System.TimeIt (timeItT)
import Data.Foldable (for_)

import IntegrateParallel (integrateParallel)

main :: IO ()
main = do
  putStrLn $ "Available number of workers: " ++ show numCapabilities
  for_ [1..numCapabilities] printTimings
  where
    printTimings workerCount = do
      (time, result) <- timeItT $ integrateParallel f start end epsilon workerCount
      putStrLn $ "workers=" ++ show workerCount ++ "\n\ttime: " ++ show time ++ "\n\tresult: " ++ show result
      where
        f = (**3)
        start = -30
        end = 32
        epsilon = 10e-8
