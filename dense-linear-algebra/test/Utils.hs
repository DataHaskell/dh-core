
module Utils where

import Data.Vector.Unboxed as U
import Prelude as P
import Data.Decimal
import GHC.Word

import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Types as T
import qualified Statistics.Matrix.Algorithms as Alg
import qualified Statistics.Matrix.Function as Func
import qualified Statistics.Matrix.Mutable as Mut

import qualified Fixtures as F


-- | Operations on vectors

-- | Operations on matrices

-- | Check if two matrices are equal within some precision
isEqual :: Word8 -> T.Matrix -> T.Matrix -> Bool
isEqual prec a b =
    let
        a' = makeDecimal prec a
        b' = makeDecimal prec b
    in
        a' == b'


------------------------------ matrix property checks --------------------------------------


-- | Check if given matrix is orthogonal
isOrtho :: T.Matrix -> Bool
isOrtho mat = isEqual 8 (M.multiply mat (M.transpose mat)) F.matId

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

-- | to check if the given matrix is invertible
isInvertible :: T.Matrix -> Bool
isInvertible mat = isFloatZero (det mat) 0.0000001


------------------------------- matrix manipulation functions -------------------------


-- | make all values of the matrix absolute
makeAbs :: T.Matrix -> T.Matrix
makeAbs mat = M.map P.abs mat

-- | make all values of the (row-list represented matrix)
-- | decimal values with a given precision
makeDecimal :: Word8 -> T.Matrix -> [[Decimal]]
makeDecimal prec mat =
    let
        matList = M.toRowLists mat
        rowToDecimal prec ls = P.map (realFracToDecimal prec) ls
    in
        P.map (rowToDecimal prec) matList


-- | to get the determinant of a given matrix
-- | taken from here:
-- | http://michaeljgilliland.blogspot.com/2013/04/determinant-in-haskell.html
det :: T.Matrix -> Double
det mat =
    let
        detHelper [[x]] = x
        detHelper mat = P.sum [(-1)^i*x*(detHelper (getRest i mat)) | (i, x) <- P.zip [0..] (P.head mat)]
        listMatrix = M.toRowLists mat
    in
        detHelper listMatrix

-- |  getRest function simply returns the matrix without the head row (topmost) and without the ith column
getRest :: Int -> [[Double]] -> [[Double]]
getRest col mat =
    let
        decapitatedMat = P.tail mat
    in
        P.map (leaveElement col) decapitatedMat


----------------------------- auxiliary helper functions -----------------------------------


-- | function to delete i'th element from list
leaveElement :: Ord a => Int -> [a] -> [a]
leaveElement _ [] = []
leaveElement i (x:xs)
        | i == 0    = xs
        | otherwise = x : leaveElement (i-1) xs

-- | compare a double value to some epsilon
isFloatZero :: Double -> Double -> Bool
isFloatZero n eps =
    if (abs n) > (abs eps)
        then True
    else False
