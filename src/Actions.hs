module Actions (destroyPoints) where

import Data.Word
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder

destroyPoints :: [Word32] -> ByteString
destroyPoints points = toLazyByteString $
                       word16LE numPoints
  where numPoints = fromIntegral (length points)
