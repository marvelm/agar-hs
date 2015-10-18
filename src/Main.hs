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
  WS.forkPingThread conn 2

  parseMessage conn =<< WS.receiveDataMessage conn

azor :: (a -> [a] -> [a]) -> [a] -> [a]
azor f elements = foldl' xs elements [0 .. length elements]
  where
    xs elems i = foldl' (`go` i) elems [0 .. length elems]
    go elems i _ = f (elems !! i) elems

main :: IO ()
main = do
  gs <- newTVarIO GameServer { gsConnections = []
                             , gsCells = [Cell { cId = 1
                                               , cPos = Position {pX = 10, pY = 10}
                                               , cMouse = Position {pX = 10, pY = 10}
                                               , cMass = 5
                                               , cType = 2
                                               , cAngle = 0.1
                                               , cIgnoreCollision = False
                                               , cIsRecombining = False
                                               }]
                             , gsCanvas = Canvas { cLeft = 0
                                                 , cBottom = 100
                                                 , cRight = 100
                                                 , cTop = 0
                                                 }
                             }
  forkIO $ forever $ do
    threadDelay 50000
    let update gs = do
          let cells = gsCells gs
          cellsArr <- newListArray (0, length cells - 1) cells :: ST s (STArray s Int Cell)
          cells' <- getElems =<< moveCells cellsArr
          return gs { gsCells = cells' }
    atomically $ modifyTVar gs $ \gs -> runST (update gs)

  forkIO $ forever $ do
    threadDelay $ 2 * 1000000
    gs' <- readTVarIO gs
    let cells = gsCells gs'
    let canvas = gsCanvas gs'

    let printCell out cell =
          out ++ show (cMass cell)

    putStrLn $ foldl' printCell "" cells

  WS.runServer "127.0.0.1" 8000 $ application gs
