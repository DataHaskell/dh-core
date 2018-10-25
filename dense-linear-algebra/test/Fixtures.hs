
module Fixtures where

import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Types as T

f :: Int -> Int -> Double
f a b = (c + d) / (d + 1) where
  c = fromIntegral a
  d = fromIntegral b

g :: Int -> Int -> Double
g a b = (c + d) / (c * d + 1) where
  c = fromIntegral a
  d = fromIntegral b

matA :: T.Matrix
matA = M.generate 3 3 f

matB :: T.Matrix
matB = M.generate 3 3 g

matId :: T.Matrix
matId = M.ident 3


