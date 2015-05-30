{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.WebSockets as WS
import Control.Monad (forever)
import Parse

application :: WS.PendingConnection -> IO()
application pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 10
  forever $
    parseMessage conn =<< WS.receiveDataMessage conn


main :: IO ()
main = do
  putStrLn "Running server."
  WS.runServer "127.0.0.1" 8000 application
