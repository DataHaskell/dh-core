{-# LANGUAGE BangPatterns #-}

module Statistics.Matrix.Fast (
    multiply
    ) where

import Prelude hiding (exponent, map)
import Control.Applicative ((<$>))
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed   ((!))
import qualified Data.Vector.Unboxed.Mutable as UM

import Statistics.Matrix.Function
import Statistics.Matrix.Types
import Statistics.Matrix.Mutable  (unsafeNew,unsafeWrite,unsafeFreeze)


multiply :: Matrix -> Matrix -> Matrix
multiply m1@(Matrix r1 _ _) m2@(Matrix _ c2 _) = runST $ do
  m3 <- unsafeNew r1 c2
  for 0 c2 $ \j -> do
    for 0 r1 $ \i -> do
      let 
        z = accum i m1 j m2 
      unsafeWrite m3 i j z
  unsafeFreeze m3

accum :: Int -> Matrix -> Int -> Matrix -> Double
accum ithrow (Matrix r1 c1 v1) jthcol (Matrix _ c2 v2) = sub 0 0
  where sub !acc !ij | ij == r1 = acc
                     | otherwise = sub ( valRow*valCol + acc ) (ij+1)
                                   where 
                                    valRow = U.unsafeIndex v1 (ithrow*c1 + ij)
                                    valCol = U.unsafeIndex v2 (ij*c2+jthcol)