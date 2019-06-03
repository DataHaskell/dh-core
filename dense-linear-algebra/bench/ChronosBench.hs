module Main where

import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Fast as F
import qualified Statistics.Matrix.Fast.Algorithms as FA
import           Statistics.Matrix (Matrix (..))
import qualified Statistics.Matrix.Algorithms as A

import qualified Data.Vector.Unboxed         as U
import           Data.Vector.Unboxed         (Vector)

import qualified System.Random.MWC as Mwc

import qualified Chronos.Bench as C

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

runtimelight :: Vector Double -> Matrix -> IO ()
runtimelight v a = do
    let 
      v2 = U.take n v
    
    C.defaultMainWith (C.defaultConfig {C.timeout = Just 3}) [
                   C.bench "norm" M.norm v2,
                   C.bench "Fast.norm" F.norm v2,

                   C.bench "multiplyV" (M.multiplyV a) (v2),
                   C.bench "Fast.multiplyV" (F.multiplyV a) (v2),
            
                   C.bench "transpose" M.transpose a ,
                   C.bench "ident" M.ident n,
                   C.bench "diag" M.diag v2
                   ]

runtimeheavy :: Matrix -> Matrix -> IO ()
runtimeheavy a b = do
    
    C.defaultMainWith (C.defaultConfig {C.timeout = Just 1}) [
                   C.bench "multiply" (M.multiply a) b,
                   C.bench "Fast.multiply" (F.multiply a) b,
                   C.bench "qr" A.qr a,
                   C.bench "Fast.qr" FA.qr a
                   ]


main :: IO ()
main = do
    v <- testVector
    a <- testMatrix
    b <- testMatrix

    --
    putStrLn "---Benchmarking light operations---"
    -- we split heavy and light, we lose some precision in the bar plots from chronos
    runtimelight v a
    putStrLn "---Benchmarking heavy operations---"
    runtimeheavy a b