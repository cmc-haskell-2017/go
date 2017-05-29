module Main where

import Control.Concurrent (forkIO)
import Network.Wai.Handler.Warp (run)
import System.Environment

import Go.Server

main :: IO ()
main = do
  [port] <- getArgs
  config <- defaultConfig
  _      <- forkIO $ periodicUpdates 10000 config
  run (read port :: Int) $ server config