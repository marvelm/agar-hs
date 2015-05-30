module Models ( Position
              , Cell
              , Player
              , Leaderboard
              , serializeLeaderboard
              ) where

import Data.ByteString.Lazy (ByteString, empty, append)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf32LE)
import Data.ByteString.Builder hiding (drop)
import Data.Word (Word32)

type Position = (Float, Float)

data Cell = Food Position
          | Virus { virusPos :: Position
                  , virusSize :: Int }

-- TODO Add player IP
data Player = Player { plId :: String
                     , plNickname :: String
                     , plScore :: Int
                     , plMass :: Int
                     , plPosition :: Position
                     , plActive :: Bool }

-- Id, Name
type Leaderboard = [(Word32, Text)]

serializeLeaderboard :: Leaderboard -> ByteString
serializeLeaderboard lb = foldr append empty ser
  where ser = map (\(_id, text) ->
                    append (toLazyByteString (word32LE _id)) (encodeUtf32LE text)) lb
