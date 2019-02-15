{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import qualified Analyze                  as A
import           Analyze.Common           ((<&>))
import qualified Data.HashSet             as HS

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

                   valueGenDouble ValueTypeDouble = ValueDouble <$> arbitrary -- we will only ouput ValueDouble

                   valueDeclGenDouble = declGen nameGen (elements [ValueTypeDouble])

                   doubleRFrameUpdateGen = valueDeclGenDouble >>= rframeUpdateGen valueGenDouble -- a frame generator that will only have Double's as data

testDrop :: Property
testDrop = monadicIO $ 
                do update <- pick (arbitrary :: Gen (RFrameUpdate Text Value)) -- generate random update
                   
                   let keys = A._rframeUpdateKeys update
                   pre $ not (null keys)

                   index <- pick $ choose (0, length keys -1)
                   original <- run $ A.fromUpdate update -- converts update to frame

                   let
                      oldData = _rframeUpdateData update 
                      newData = rmv index <$> oldData
                      newKeys = rmv index keys
                      expectedUpdate = update{ _rframeUpdateData = newData, _rframeUpdateKeys = newKeys}
                      
                   expected <- run $ A.fromUpdate expectedUpdate

                   let assertion1 = A.numCols original -1 == A.numCols expected

                   let keyToRemove = HS.singleton $ keys V.! index
                       actual =  A.dropCols (`HS.member` keyToRemove) original
                       assertion2 = A._rframeKeys actual == A._rframeKeys expected

                   assert $ assertion1 && assertion2

                where
                   rmv index vec = V.take index vec V.++ V.drop (index+1) vec 
                   
testKeep :: Property
testKeep = monadicIO $ 
                do update <- pick (arbitrary :: Gen (RFrameUpdate Text Value)) -- generate random update
                   
                   let keys = A._rframeUpdateKeys update
                   pre $ not (null keys)

                   index <- pick $ choose (0, length keys -1) 
                   len <- pick $ choose (1, length keys - index) -- picks a length of slice 

                   original <- run $ A.fromUpdate update -- converts update to frame

                   let
                      oldData = _rframeUpdateData update 
                      newData = V.slice index len <$> oldData
                      newKeys = V.slice index len keys
                      expectedUpdate = update{ _rframeUpdateData = newData, _rframeUpdateKeys = newKeys}
                      
                   expected <- run $ A.fromUpdate expectedUpdate

                   let assertion1 = A.numCols original == A.numCols expected + (length keys) - len

                   let keyToKeep = HS.fromList $ V.toList newKeys
                       actual = A.keepCols (`HS.member` keyToKeep) original
                       assertion2 = A._rframeKeys actual == A._rframeKeys expected

                   assert $ assertion1 && assertion2


propTests :: Ts.TestTree
propTests = Ts.testGroup "Test suite"
  [ QC.testProperty "Fixture" testFixture,
    QC.testProperty "Row Decode" testRowDecode,
    QC.testProperty "Drop" testDrop,
    QC.testProperty "Keep" testKeep
  ]

main :: IO ()
main = Ts.defaultMain propTests