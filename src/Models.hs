module Models ( Position
              , Cell
              , Player
              , Leaderboard
              , serializeLeaderboard
              ) where

import Data.ByteString.Lazy (ByteString, empty, append)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf16LE)
import Data.ByteString.Builder
import Data.Word (Word32)
import Data.Monoid

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
                     , plActive :: Bool
                     }

data Canvas = Canvas { cLeft :: Float
                     , cBottom :: Float
                     , cRight :: Float
                     , cTop :: Float
                     }

serializeCanvas :: Canvas -> ByteString
serializeCanvas canvas = toLazyByteString $
                         prefix <>
                         floatLE (cLeft canvas) <>
                         floatLE (cBottom canvas) <>
                         floatLE (cRight canvas) <>
                         floatLE (cTop canvas)
  where prefix = word8 64

-- | Id, Name
type Leaderboard = [(Word32, Text)]

serializeLeaderboard :: Leaderboard -> ByteString
serializeLeaderboard leaderboard = toLazyByteString $
                                   prefix <>
                                   word32LE target <>
                                   lazyByteString (foldr append empty serialized)
  where serialized = map serialize leaderboard
        serialize (_id, text) = toLazyByteString $
                                word32LE _id <>
                                lazyByteString (encodeUtf16LE text) <>
                                endText

        target = fromIntegral (length leaderboard - 1) :: Word32
        prefix = word8 49
        endText = word16LE 0
