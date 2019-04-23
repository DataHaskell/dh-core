
module Fixtures where

import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Types as T

f :: Int -> Int -> Double
f a b = (c + d + 1) / (d + 1) where
  c = fromIntegral a
  d = fromIntegral b

g :: Int -> Int -> Double
g a b = (c + d + 1) / (c * d + 1) where
  c = fromIntegral a
  d = fromIntegral b

c :: [[Double]]
c = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]]

d :: [[Double]]
d = [[3.0, 2.5, 1.6], [5.5, 2.3, 6.9], [3.7, 9.1, 10.0]]

matA :: T.Matrix
matA = M.generate 3 3 f

matB :: T.Matrix
matB = M.generate 3 3 g

matC :: T.Matrix
matC = M.fromRowLists c

matD :: T.Matrix
matD = M.fromRowLists d

matId :: T.Matrix
matId = M.ident 3


