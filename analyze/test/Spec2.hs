{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import qualified Analyze                  as A
import           Analyze.Common           ((<&>))

import Analyze.RFrame (RFrame (..), RFrameUpdate (..))
import Analyze.Values

import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

import Generation

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run, pick, pre)

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
                do update <- pick (arbitrary :: Gen (RFrameUpdate Text Value)) -- generate random update
                   frame <- run $ A.fromUpdate update -- converts update to frame
                   let
                      -- get keys from both the update and the frame. We'll then check that both give the same result
                      keys = A._rframeKeys frame
                      keysUp = A._rframeUpdateKeys update 

                      nbRows = A.numRows frame
                      nbRowsUp = length $ A._rframeUpdateData update

                      nbCols = A.numCols frame
                      nbColsUp = length $ A._rframeUpdateKeys update
                   assert $ (keys == keysUp) && (nbRows == nbRowsUp) && (nbCols == nbColsUp)

testRowDecode :: Property
testRowDecode = monadicIO $ 
                do update <- pick (doubleRFrameUpdateGen) -- we only want RFrameUpdate with Double as data
                   pre $ not (null (A._rframeUpdateKeys update))
                   index <- pick $ choose (0, length (A._rframeUpdateKeys update) -1) -- we chose a random index in the range of the update's keys
                   frame <- run $ A.fromUpdate update

                   let
                      resultUpdate = (*2) . extractDouble . (V.! index) <$> A._rframeUpdateData update

                      key = (A._rframeKeys frame) V.! index

                      decoder = A.requireWhere key A.floating <&> (*2) :: A.Decoder IO Text Value Double 

                   resultFrame <- run $ sequenceA =<< A.decode decoder frame

                   assert $ resultFrame == resultUpdate 

                where
                   extractDouble (ValueDouble double) = double

                   valueGenDouble x = ValueDouble <$> arbitrary -- we ignore the type that is given, we will output a double anyway

                   valueDeclGenDouble = declGen nameGen (elements [ValueTypeDouble]) -- for some reason, specifying ValueTypeDOuble here isn't enough to guarantee that only ValueDouble will be generated

                   doubleRFrameUpdateGen = valueDeclGen >>= rframeUpdateGen valueGenDouble -- a frame generator that will only have Double's as data

propTests :: Ts.TestTree
propTests = Ts.testGroup "Test suite"
  [ QC.testProperty "Fixture" testFixture,
    QC.testProperty "Row Decode" testRowDecode
  ]

main :: IO ()
main = Ts.defaultMain propTests