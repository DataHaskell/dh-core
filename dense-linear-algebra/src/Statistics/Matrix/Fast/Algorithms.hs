{-# LANGUAGE BangPatterns #-}

-- |
-- Module    : Statistics.Matrix.Algorithms
-- Copyright : 2014 Bryan O'Sullivan
-- License   : BSD3
--
-- Useful matrix functions.

module Statistics.Matrix.Fast.Algorithms
    (
      qr
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.ST (ST, runST)
import Prelude hiding (replicate)
import Statistics.Matrix (Matrix (..),dimension, for)
import qualified Statistics.Matrix.Mutable as M
import qualified Data.Vector.Unboxed as U

-- | /O(r*c)/ Compute the QR decomposition of a matrix.
-- The result returned is the matrices (/q/,/r/).
qr :: Matrix -> (Matrix, Matrix)
qr mat = runST $ do
  let (m,n) = dimension mat

  r <- M.replicate n n 0
  a <- M.thaw mat
  for 0 n $ \j -> do
    cn <- M.immutably a $ \aa -> sqrt $ normCol j aa
    M.unsafeWrite r j j cn
    for 0 m $ \i -> M.unsafeModify a i j (/ cn)
    for (j+1) n $ \jj -> do
      p <- innerProduct a j jj
      M.unsafeWrite r j jj p
      for 0 m $ \i -> do
        aij <- M.unsafeRead a i j
        M.unsafeModify a i jj $ subtract (p * aij)
  (,) <$> M.unsafeFreeze a <*> M.unsafeFreeze r

normCol :: Int -> Matrix -> Double
normCol jthcol (Matrix r c v) = sub 0 0
  where sub !acc !ij | ij == r = acc
                     | otherwise = sub ( valCol*valCol + acc ) (ij+1)
                                   where
                                    valCol = U.unsafeIndex v (ij*c+jthcol)

innerProduct :: M.MMatrix s -> Int -> Int -> ST s Double
innerProduct mmat j k = M.immutably mmat $ \mat ->
  dotCol j mat k mat

dotCol :: Int -> Matrix -> Int -> Matrix -> Double
dotCol jthcol (Matrix r1 c1 v1) kthcol (Matrix _ c2 v2) = sub 0 0
  where sub !acc !ij | ij == r1 = acc
                     | otherwise = sub ( valColj*valColk + acc ) (ij+1)
                                   where
                                    valColk = U.unsafeIndex v2 (ij*c2+kthcol)
                                    valColj = U.unsafeIndex v1 (ij*c1+jthcol)