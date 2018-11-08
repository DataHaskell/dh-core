
module AlgorithmsSpec where

import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Types as T
import qualified Statistics.Matrix.Algorithms as Alg
import qualified Statistics.Matrix.Function as Func
import qualified Statistics.Matrix.Mutable as Mut

import Fixtures as F
import Utils

qrDecompositionInvariant :: Bool
qrDecompositionInvariant =
  (M.multiply (fst res) (snd res)) == F.matA
  where
    res = Alg.qr F.matA


qrFirstOrthoInvariant :: Bool
qrFirstOrthoInvariant = Utils.isOrtho (fst res)
  where
    res = Alg.qr F.matA

qrSecondTriInvariant :: Bool
qrSecondTriInvariant = Utils.isUpperTri (snd res)
  where
    res = Alg.qr F.matA


