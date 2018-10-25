
module Utils where

import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Types as T
import qualified Statistics.Matrix.Algorithms as Alg
import qualified Statistics.Matrix.Function as Func
import qualified Statistics.Matrix.Mutable as Mut

-- | Check if given matrix is identity matrix
isIdentity :: T.Matrix -> Bool

-- | Check if given matrix is orthogonal
isOrtho :: T.Matrix -> Bool

-- | Check if given matrix is upper triangular
isUpperDelta :: T.Matrix -> Bool

-- | Check if given matrix is lower triangular
isLowerDelta :: T.Matrix -> Bool

-- | Check if a matrix is invertible
isInvertible :: T.Matrix -> Bool
