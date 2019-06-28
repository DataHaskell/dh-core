{-# LANGUAGE BangPatterns #-}

module Statistics.Matrix.Fast (
    multiply,
    norm,
    multiplyV,
    transpose
    ) where

import Prelude hiding (exponent, map)
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Statistics.Matrix (row)
import Statistics.Matrix.Function
import Statistics.Matrix.Types
import Statistics.Matrix.Mutable  (unsafeNew,unsafeWrite,unsafeFreeze)

-- | Matrix-matrix multiplication in a more imperative fashion. Matrices must be of compatible
-- sizes (/note: not checked/). Faster but less accurate than Statistics.Matrix.multiply
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

-- | Matrix-vector multiplication, with better performances but not as accurate as
-- Statistics.Matrix.multiplyV
multiplyV :: Matrix -> Vector -> Vector
multiplyV m v
  | cols m == c = U.generate (rows m) (U.sum . U.zipWith (*) v . row m)
  | otherwise   = error $ "matrix/vector unconformable " ++ show (cols m,c)
  where c = U.length v

-- | Norm of a vector. Faster but less accurate than Statistics.Matrix.norm
norm :: Vector -> Double
norm = sqrt . U.sum . U.map square

transpose :: Matrix -> Matrix
transpose (Matrix r0 c0 v0) 
  = Matrix c0 r0 $ runST $ do
    vec <- UM.unsafeNew (r0*c0)
    for 0 r0 $ \i -> do
      UM.unsafeWrite vec (i + i * c0) $ v0 `U.unsafeIndex` (i + i * c0)
      for (i+1) c0 $ \j -> do
        let tmp = v0 `U.unsafeIndex` (j + i * c0)
            tmp2 = v0 `U.unsafeIndex` (i + j * c0)
        UM.unsafeWrite vec (j + i * c0) tmp2
        UM.unsafeWrite vec (i + j * c0) tmp
    U.unsafeFreeze vec
