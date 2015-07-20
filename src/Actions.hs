module Actions ( destroyPoints
               , moveCells
               ) where

import Data.Word
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder
import Data.Monoid
import Data.Foldable
import Data.Array.ST
import Data.Array.MArray
import Control.Monad.ST
import Models

destroyPoints :: [Word32] -> ByteString
destroyPoints points = toLazyByteString $
                       word16LE numPoints
  where numPoints = fromIntegral (length points)

-- | 'moveCells' transforms an MArray of Cell
-- It should be passed into 'runST' to get an MArray of Cell
moveCells cells = do

  {- REVIEW
- Is returning an Array is necessary?
    This function should mutate 'cells' and the caller should be able to continue using the array that was passed in.
  -}

  (start, stop) <- getBounds cells
  let ixes = [start .. stop]
  forM_ ixes $ \i -> do
    cell <- readArray cells i
    let
      getX :: Cell -> Float
      getX = fromIntegral . pX . cPos
      getY :: Cell -> Float
      getY = fromIntegral . pY . cPos

      x2 = (fromIntegral . pX . cMouse) cell
      y2 = (fromIntegral . pY . cMouse) cell

      x = getX cell
      y = getY cell

      dY = y2 - y
      dX = x2 - x

      angle :: Float
      angle = atan2 dX dY

      dist :: Float
      dist = sqrt $ (dX ** 2) + (dY ** 2)

      mass :: Float
      mass = fromIntegral $ cMass cell

      speed :: Float
      speed = 745.28 * (mass ** (-0.222)) * (50 / 1000)

      speed' :: Float
      speed' = min speed dist

      x1 = x + (speed' * sin angle)
      y1 = y + (speed' * cos angle)
      in

     forM_ ixes $ \j -> do
       otherCell <- readArray cells j
       let
         notRecombining = not . cIsRecombining
         otherCell'
           | otherCell == cell = cell -- Do nothing if 'otherCell' is the same
           | cIgnoreCollision cell = otherCell

           | (notRecombining otherCell) || (notRecombining cell) &&
             dist < collisionDist &&
             not collisionCheck =
               otherCell { cPos = position }

           | otherwise = otherCell

         dist = sqrt $ ((getX otherCell - getX cell) ** 2) + ((getY otherCell - getY cell) ** 2)
         collisionDist = fromIntegral $ cSize otherCell + cSize cell

         cellBorder = 2 * collisionDist
         collisionCheck = abs (x - getX otherCell) < cellBorder &&
                          abs (y - getY otherCell) < cellBorder

         dX = getX otherCell - x1
         dY = getY otherCell - y1
         angle = atan2 dX dY
         move = collisionDist - dist
         otherPos = cPos otherCell
         position = Position { pX = pX otherPos + truncate (move * sin angle)
                             , pY = pY otherPos + truncate (move * cos angle) }

         borderLeft = 0
         borderRight = 1000
         borderTop = 0
         borderBottom = 1000

         x1'
           | x1 < borderLeft = borderLeft
           | x1 > borderRight = borderRight
           | otherwise = x1

         y1'
           | y1 < borderTop = borderTop
           | y1 > borderBottom = borderBottom
           | otherwise = y1

         cell' = cell { cPos = Position { pX = truncate x1', pY= truncate y1' } }
         in
        writeArray cells j otherCell'
  return cells
