{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.WebSockets as WS
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)
import Parse
import Models

application :: WS.PendingConnection -> IO()
application pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 10

  let lb = [(1, "ayy lmao"), (2, "doge"), (3, "CIA")]

  forkIO $ forever $ do
    threadDelay 1000000
    WS.sendDataMessage conn $ WS.Binary (serializeLeaderboard lb)

  parseMessage conn =<< WS.receiveDataMessage conn


main :: IO ()
main = do
  putStrLn "Running server."
  WS.runServer "127.0.0.1" 8000 application
