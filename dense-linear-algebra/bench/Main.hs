{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Statistics.Matrix as M
import           Statistics.Matrix (Matrix (..))
import qualified Statistics.Matrix.Algorithms as A

import qualified Data.Vector.Unboxed         as U
import           Data.Vector.Unboxed         (Vector)

import qualified System.Random.MWC as Mwc

import qualified Chronos.Bench as C
import qualified Weigh         as W 

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

deriving instance Generic Matrix
deriving instance NFData Matrix

n :: Int
n = 100

testVector :: IO (Vector Double)
testVector = do 
    gen <- Mwc.create
    Mwc.uniformVector gen (n^2)


testMatrix :: IO Matrix
testMatrix = do
    vec <- testVector
    return $ Matrix n n vec

runtimelight :: Vector Double -> Matrix -> Matrix -> IO ()
runtimelight v a b = do 
    let 
      v2 = U.take n v
    
    C.defaultMainWith (C.defaultConfig {C.timeout = Just 3}) [
                   C.bench "norm" M.norm v,

                   C.bench "multiplicationV" (M.multiplyV a) (v2),
            
                   C.bench "transpose" M.transpose a ,
                   C.bench "identity" M.ident n,
                   C.bench "diag" M.diag v2
                   ]

runtimeheavy :: Vector Double -> Matrix -> Matrix -> IO ()
runtimeheavy v a b = do 
    let 
      v2 = U.take n v
    
    C.defaultMainWith (C.defaultConfig {C.timeout = Just 1}) [
                   C.bench "multiplication" (M.multiply a) b,
                   C.bench "qr factorization" A.qr a
                   ]

weight :: Vector Double -> Matrix -> Matrix -> IO ()
weight v a b = do
    let 
      v2 = U.take n v
    W.mainWith (do 
        W.func "norm" M.norm v

        W.func "multiplication" (M.multiply a) b
        W.func "multiplicationV" (M.multiplyV a) (v2)
        W.func "qr factorization" A.qr a

        W.func "transpose" M.transpose a
        W.func "identity" M.ident n
        W.func "diag" M.diag v2)



main :: IO ()
main = do
    v <- testVector
    a <- testMatrix
    b <- testMatrix

    --
    print "---Benchmarking light operations---"
    runtimelight v a b
    print "---Benchmarking heavy operations---"
    runtimeheavy v a b
    print "---Benchmarking memory consumption---"
    weight v a b