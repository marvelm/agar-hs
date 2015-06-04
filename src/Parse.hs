{-# LANGUAGE OverloadedStrings #-}
module Parse (parseMessage) where

import Models
import Data.ByteString.Lazy ( ByteString, take, drop, length
                            , isSuffixOf, isPrefixOf)
import Prelude hiding (take, drop, length)
import Data.ByteString.Builder
import Data.Monoid ((<>))
import Data.Text.Lazy.Encoding (decodeUtf16LE)
import Data.Text.Lazy (Text)
import Data.Binary.IEEE754
import Data.Binary.Get
import qualified Network.WebSockets as WS

greeting :: ByteString
greeting = toLazyByteString $
           word8 255 <>
           word32LE 1

spectate :: ByteString
spectate = toLazyByteString $
           word8 1

split :: ByteString
split = toLazyByteString $
        word8 17

ejectMass :: ByteString
ejectMass = toLazyByteString $
            word8 21

isName :: ByteString -> Bool
isName bs = namePrefix `isPrefixOf` bs
  where namePrefix = toLazyByteString $ word8 0

parseName :: ByteString -> Maybe Text
parseName bs | isName bs = Just $ decodeUtf16LE $ drop 1 bs
parseName _ = Nothing

isLocation :: ByteString -> Bool
isLocation bs = (locationPrefix `isPrefixOf` bs) &&
                (locationSuffix `isSuffixOf` bs)
  where locationPrefix = toLazyByteString $ word8 16
        locationSuffix = toLazyByteString $ floatLE 0

parseLocation :: Get (Double, Double)
parseLocation = do
  prefix <- getWord8
  xPos <- getFloat64le
  yPos <- getFloat64le
  suffix <- getWord32le
  if (prefix == locPrefix) && (suffix == locSuffix)
     then return (xPos, yPos)
     else fail "bad"
  where locPrefix = 16
        locSuffix = 0

parseMessage :: WS.Connection -> WS.DataMessage -> IO ()
parseMessage conn (WS.Text t)
  | t == "Ping" =
    WS.sendDataMessage conn $ WS.Text "Pong"

parseMessage conn (WS.Binary b)
  | b == greeting = do
      putStrLn "Received greeting"
      WS.sendDataMessage conn $ WS.Text "Hello :)"

parseMessage conn (WS.Binary b)
  | b == spectate = do
      putStrLn "Player is spectating"
      WS.sendDataMessage conn $ WS.Text "Spectating"

parseMessage conn (WS.Binary b)
  | b == split = do
      putStrLn "Player has split"
      WS.sendDataMessage conn $ WS.Text "Split"

parseMessage conn (WS.Binary b)
  | b == ejectMass = do
      putStrLn "Player has ejected some mass"
      WS.sendDataMessage conn $ WS.Text "Ejected mass"

parseMessage _ (WS.Binary b)
  | isLocation b =
      case runGetOrFail parseLocation b of
       Right (_, _, (x, y)) ->
         print (x, y)

-- TODO Fix this
parseMessage _ (WS.Binary b)
  | isName b =
      case parseName b of
       Just name ->
         print name

parseMessage _ msg = print msg

-- UNIMPLEMENTED
quitKeyDown :: ByteString
quitKeyDown = toLazyByteString $
              word8 18

-- UNIMPLEMENTED
quitKeyUp :: ByteString
quitKeyUp = toLazyByteString $
            word8 19
