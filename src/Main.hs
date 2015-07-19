{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.WebSockets as WS
import Control.Monad (forever)
import Control.Monad.ST
import Control.Monad.STM
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar
import Parse
import Models
import Actions
import Data.ByteString.Builder
import Data.List (foldl')
import Data.Monoid
import Data.Array.ST
import Data.Array

application :: TVar GameServer -> WS.PendingConnection -> IO ()
application gs pending = do
  conn <- WS.acceptRequest pending
  gs' <- readTVarIO gs
  WS.forkPingThread conn 10

  parseMessage conn =<< WS.receiveDataMessage conn

azor :: (a -> [a] -> [a]) -> [a] -> [a]
azor f elements = foldl' xs elements [0..length elements]
  where
    xs elems i   = foldl' (`go` i) elems [0..length elems]
    go elems i _ = f (elems !! i) elems

main = do
  gs <- newTVarIO GameServer { gsConnections = []
                             , gsCells = []
                             }
  forkIO $ forever $ do
    threadDelay 50000
    let update gs = do
          let cells = gsCells gs
          cellsArr <- newListArray (0, length cells - 1) cells :: ST s (STArray s Int Cell)
          cells' <- getElems =<< moveCells cellsArr
          return gs { gsCells = cells' }
    atomically $ modifyTVar gs $ \gs -> runST (update gs)

  WS.runServer "127.0.0.1" 8000 $ application gs
