module Main where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (for_)
import GHC.Conc (numCapabilities)
import IntegrateParallel (integrateParallel)
import Utils (timeIoAction)

main :: IO ()
main = do
  putStrLn $ "Available number of workers: " ++ show numCapabilities
  for_ [1 .. numCapabilities] printTimings
  where
    printTimings workerCount = do
      putStrLn $ "workers=" ++ show workerCount
      (time, result) <- timeIoAction $ integrateParallel f start end epsilon workerCount
      putStrLn $ "\ttime: " ++ show time
      putStrLn $ "\tresult: " ++ show result
      where
        f = ((* (-0.05)) . (** 3)) . (** 2)
        start = -10
        end = 10
        epsilon = 10e-8
