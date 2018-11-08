
module Utils where

import Data.Vector.Unboxed as U
import Prelude as P

import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Types as T
import qualified Statistics.Matrix.Algorithms as Alg
import qualified Statistics.Matrix.Function as Func
import qualified Statistics.Matrix.Mutable as Mut

import qualified Fixtures as F


-- | Operations on vectors

-- | Checks that first n elements of a vector are zero
-- by mapping an "isZero" function to the first N elements
-- and then folding the resultant vector with AND
firstNzero :: T.Vector -> Int -> Bool
firstNzero vec n = U.foldl (&&) True $ U.map (== 0) $ U.take n vec

-- | Operations on matrices

-- | Check if given matrix is identity matrix
isIdentity :: T.Matrix -> Bool
isIdentity mat = (M.multiply mat F.matId) == F.matId

-- | Check if given matrix is orthogonal
isOrtho :: T.Matrix -> Bool
isOrtho mat = M.multiply mat (M.transpose mat) == F.matId


-- | Check if given matrix is upper triangular
isUpperTri :: T.Matrix -> Bool
isUpperTri mat = P.foldl (&&) True $ P.map firstNinRow [1..m]
  where
    -- to check that the first n elements
    -- of row n of the matrix are zero
    firstNinRow n = firstNzero (M.row mat n) n
    -- to get the dimensions of the matrix
    m = fst $ M.dimension mat -- rows


-- | Check if given matrix is lower triangular
isLowerTri :: T.Matrix -> Bool
isLowerTri mat = P.foldl (&&) True $ P.map firstNinColumn [1..m]
  where
    -- to check that the first n elements
    -- of column n of the matrix are zero
    firstNinColumn n = firstNzero (M.column mat n) n
    -- to get the dimensions of the matrix
    m = snd $ M.dimension mat -- columns
