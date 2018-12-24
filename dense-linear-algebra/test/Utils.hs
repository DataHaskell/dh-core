
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

-- | Operations on matrices

-- | Check if given matrix is identity matrix
isIdentity :: T.Matrix -> Bool
isIdentity mat = (M.multiply mat F.matId) == F.matId

-- | Check if given matrix is orthogonal
isOrtho :: T.Matrix -> Bool
isOrtho mat = M.multiply mat (M.transpose mat) == F.matId


-- | Checks if the n'th row of the matrix have 'n' leading zeros
hasLeadingZeros :: Int -> T.Vector -> Bool
hasLeadingZeros n row = U.foldl (&&) True (U.map (== 0) (U.take n row))

-- | pass the n'th row of the matrix to hasLeadingZeros
isRowFromTriMatrix :: T.Matrix -> Int -> Bool
isRowFromTriMatrix mat n = hasLeadingZeros n (M.row mat n)

-- | Helper function to check if the given matrix is upper triangular
upperTriHelper :: T.Matrix -> Int -> Bool
upperTriHelper mat n = P.foldl (&&) True $ P.map (isRowFromTriMatrix mat) [0..(n-1)]

-- | to check if the given matrix is upper triangular
isUpperTri :: T.Matrix -> Bool
isUpperTri mat = upperTriHelper mat (fst (M.dimension mat))
