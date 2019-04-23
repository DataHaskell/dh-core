
module AlgorithmsSpec where

import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Types as T
import qualified Statistics.Matrix.Algorithms as Alg
import qualified Statistics.Matrix.Function as Func
import qualified Statistics.Matrix.Mutable as Mut

import Utils

qrDecompositionInvariant :: T.Matrix -> Bool
qrDecompositionInvariant mat =
  (M.multiply (fst res) (snd res)) == mat
  where
    res = Alg.qr mat


qrFirstOrthoInvariant :: T.Matrix -> Bool
qrFirstOrthoInvariant mat = Utils.isOrtho (fst res)
  where
    res = Alg.qr mat

qrSecondTriInvariant :: T.Matrix -> Bool
qrSecondTriInvariant mat = Utils.isUpperTri (snd res)
  where
    res = Alg.qr mat


