{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.WebSockets as WS
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)
import Parse
import Models
import Data.ByteString.Builder



application :: WS.PendingConnection -> IO()
application pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 10

  parseMessage conn =<< WS.receiveDataMessage conn


main :: IO ()
main = do
  putStrLn "Running server."
  WS.runServer "127.0.0.1" 8000 application
