{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import qualified Analyze as A

import Analyze.RFrame (RFrame (..), RFrameUpdate (..))
import Analyze.Values

import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

import Generation

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run, pick)

import qualified Test.Tasty            as Ts
import qualified Test.Tasty.QuickCheck as QC

rframeUpdateGenSized :: A.Data k => (t -> Gen v) -> Vector (k, t) -> Int -> Gen (RFrameUpdate k v)
rframeUpdateGenSized prod decl numRows = gen
  where
    rowGen = sequenceA (prod . snd <$> decl)
    allRowsGen = V.replicateM numRows rowGen
    keys = fst <$> decl
    gen = RFrameUpdate keys <$> allRowsGen

rframeUpdateGen :: A.Data k => (t -> Gen v) -> Vector (k, t) -> Gen (RFrameUpdate k v)
rframeUpdateGen prod decl = sized (rframeUpdateGenSized prod decl)

valueRFrameUpdateGen = valueDeclGen >>= rframeUpdateGen valueGen

instance Arbitrary (RFrameUpdate Text Value) where 
    arbitrary = valueRFrameUpdateGen

testFixture :: Property
testFixture = monadicIO $ 
                do update <- pick (arbitrary :: Gen (RFrameUpdate Text Value))
                   frame <- run $ A.fromUpdate update
                   let
                      keys = A._rframeKeys frame
                      keysUp = A._rframeUpdateKeys update 

                      nbRows = A.numRows frame
                      nbRowsUp = length $ A._rframeUpdateData update

                      nbCols = A.numCols frame
                      nbColsUp = length $ A._rframeUpdateKeys update
                   assert $ (keys == keysUp) && (nbRows == nbRowsUp) && (nbCols == nbColsUp)

propTests :: Ts.TestTree
propTests = Ts.testGroup "Test suite"
  [ QC.testProperty "Fixture" testFixture
  ]

main :: IO ()
main = Ts.defaultMain propTests