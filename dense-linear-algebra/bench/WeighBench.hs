module Main where

import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Fast as F
import qualified Statistics.Matrix.Fast.Algorithms as FA
import           Statistics.Matrix (Matrix (..))
import qualified Statistics.Matrix.Algorithms as A

import qualified Data.Vector.Unboxed         as U
import           Data.Vector.Unboxed         (Vector)

import qualified System.Random.MWC as Mwc

import qualified Weigh         as W 

n :: Int
n = 100

testVector :: IO (Vector Double)
testVector = do 
    gen <- Mwc.create
    Mwc.uniformVector gen (n*n)


testMatrix :: IO Matrix
testMatrix = do
    vec <- testVector
    return $ Matrix n n vec


weight :: Vector Double -> Matrix -> Matrix -> IO ()
weight v a b = do
    let 
      v2 = U.take n v
    W.mainWith (do 
        W.func "norm" M.norm v2
        W.func "Fast.norm" F.norm v2

        W.func "multiplyV" (M.multiplyV a) (v2)
        W.func "Fast.multiplyV" (F.multiplyV a) (v2)
        W.func "transpose" M.transpose a
        W.func "ident" M.ident n
        W.func "diag" M.diag v2

        W.func "multiply" (M.multiply a) b
        W.func "Fast.multiply" (F.multiply a) b
        
        W.func "qr" A.qr a
        W.func "Fast.qr" FA.qr a

        )



main :: IO ()
main = do
    v <- testVector
    a <- testMatrix
    b <- testMatrix

    putStrLn "---Benchmarking memory consumption---"
    weight v a b
