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

import Debug.Trace

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

rframeUpdateGenFixedSize :: A.Data k => (t -> Gen v) -> Vector (k, t) -> Int -> Gen (RFrameUpdate k v)
rframeUpdateGenFixedSize prod decl size = rframeUpdateGenSized prod decl size

valueRFrameUpdateGenFixedSize size = valueDeclGen >>= rframeUpdateGen valueGen 

instance Arbitrary (RFrameUpdate Text Value) where 
    arbitrary = valueRFrameUpdateGen

instance Arbitrary Value where
  arbitrary = valueTypeGen >>= valueGen

testFixture :: Property
testFixture = monadicIO $ 
                do update <- pick (arbitrary :: Gen (RFrameUpdate Text Value)) -- generate random update
                   frame <- run $ A.fromUpdate update 
                   let
                      -- get keys from both the update and the frame
                      keys = A._rframeKeys frame
                      keysUp = A._rframeUpdateKeys update 

                      -- gets data from both
                      nbRows = A.numRows frame
                      nbRowsUp = length $ A._rframeUpdateData update

                      -- number of colums from both
                      nbCols = A.numCols frame
                      nbColsUp = length $ A._rframeUpdateKeys update

                   -- checks everything is the same for both 
                   assert $ (keys == keysUp) && (nbRows == nbRowsUp) && (nbCols == nbColsUp)

testRowDecode :: Property
testRowDecode = monadicIO $ 
                do update <- pick (doubleRFrameUpdateGen) -- we only want RFrameUpdate with Double as data
                   frame <- run $ A.fromUpdate update -- converts `update` to frame

                   -- we check that we have more than 0 keys
                   let keys = A._rframeUpdateKeys update
                   pre $ not (null keys)

                   -- we chose a random index in the range of the `update`'s keys
                   index <- pick $ choose (0, length keys -1) 
                   

                   let
                      -- we basically map (*2) over the column indicated by `index` in `update`
                      resultUpdate = (*2) . extractDouble . (V.! index) <$> A._rframeUpdateData update

                      -- we take the key corresponding to the column
                      key = (A._rframeKeys frame) V.! index
                      decoder = A.requireWhere key A.floating <&> (*2) :: A.Decoder IO Text Value Double 
                   -- decode
                   resultFrame <- run $ sequenceA =<< A.decode decoder frame
                   
                   assert $ resultFrame == resultUpdate 

                where
                   extractDouble (ValueDouble double) = double
                   valueGenDouble ValueTypeDouble = ValueDouble <$> arbitrary -- we will only ouput ValueDouble
                   valueDeclGenDouble = declGen nameGen (elements [ValueTypeDouble])
                   doubleRFrameUpdateGen = valueDeclGenDouble >>= rframeUpdateGen valueGenDouble -- a frame generator that will only have Double's as data

testDrop :: Property
testDrop = monadicIO $ 
                do update <- pick (arbitrary :: Gen (RFrameUpdate Text Value))
                   
                   -- we check that we have more than 0 keys
                   let keys = A._rframeUpdateKeys update
                   pre $ not (null keys)

                   -- we chose a random index in the range of the `update`'s keys
                   index <- pick $ choose (0, length keys -1)
                   -- converts update to frame
                   original <- run $ A.fromUpdate update 

                   let
                      oldData = A._rframeUpdateData update 

                      newKeys = rmv index keys
                      -- we remove the elements of a column by mapping `rmv index` over every row
                      newData = rmv index <$> oldData
                      
                      expectedUpdate = update{ _rframeUpdateData = newData, _rframeUpdateKeys = newKeys}
                      
                   expected <- run $ A.fromUpdate expectedUpdate

                   -- checks that the length has decreased by exactly 1
                   let sameNbCols = A.numCols original -1 == A.numCols expected

                   -- we remove the column satisfying the condition (`HS.member` keyToRemove)
                   let 
                       keyToRemove = HS.singleton $ keys V.! index
                       actual =  A.dropCols (`HS.member` keyToRemove) original

                       sameKeys = A._rframeKeys actual == A._rframeKeys expected

                   assert $ sameNbCols && sameKeys

                where
                   -- removes the element at `index`
                   rmv index vec = V.take index vec V.++ V.drop (index+1) vec 
                   
testKeep :: Property
testKeep = monadicIO $ -- same spirit as testDrop
                do update <- pick (arbitrary :: Gen (RFrameUpdate Text Value))
                   
                   let keys = A._rframeUpdateKeys update
                   pre $ not (null keys)

                   index <- pick $ choose (0, length keys -1) 
                   -- picks a length of slice 
                   lenSlice <- pick $ choose (0, length keys - index) 

                   original <- run $ A.fromUpdate update

                   let
                      oldData = _rframeUpdateData update 
                      newData = V.slice index lenSlice <$> oldData
                      newKeys = V.slice index lenSlice keys
                      expectedUpdate = update{ _rframeUpdateData = newData, _rframeUpdateKeys = newKeys}
                      
                   expected <- run $ A.fromUpdate expectedUpdate

                   let sameNbCols = A.numCols original == A.numCols expected + (length keys) - lenSlice

                   let keyToKeep = HS.fromList $ V.toList newKeys
                       actual = A.keepCols (`HS.member` keyToKeep) original
                       sameKeys = A._rframeKeys actual == A._rframeKeys expected

                   assert $ sameNbCols && sameKeys

testUpdateEmpty :: Property
testUpdateEmpty = monadicIO $ 
                do update <- pick (arbitrary :: Gen (RFrameUpdate Text Value))

                   let
                      lengthEmpty = length $ A._rframeUpdateKeys update
                      emptyUpdate = RFrameUpdate V.empty (V.replicate lengthEmpty V.empty)

                   empty <- run $ A.fromUpdate emptyUpdate
                   actual <- run $ A.update update empty

                   expected <- run $ A.fromUpdate update

                   assert $ actual == expected

testUpdateEmpty2 :: Property
testUpdateEmpty2 = monadicIO $ 
                do frameUp <- pick (arbitrary :: Gen (RFrameUpdate Text Value))
                   frame <- run $ A.fromUpdate frameUp

                   let
                      lengthEmpty = length $ A._rframeUpdateKeys frameUp
                      emptyUpdate = RFrameUpdate V.empty (V.replicate lengthEmpty V.empty)

                   actual <- run $ A.update emptyUpdate frame

                   assert $ actual == frame

testUpdateAdd :: Property
testUpdateAdd = monadicIO $ 
                -- the general idea is to generate an update, then split it, keep one part into an update, 
                -- transform the other into a frame, and update the second with the first  
                do original <- pick (arbitrary :: Gen (RFrameUpdate Text Value))
                   expected <- run $ A.fromUpdate original

                   let originalKeys = A._rframeUpdateKeys original
                       lenKeys = length originalKeys

                   pre $ not (null originalKeys)
 
                   lenSlice <- pick $ choose (0, length originalKeys)

                   let
                      originalData = A._rframeUpdateData original

                      fstData = V.slice 0 lenSlice <$> originalData
                      fstKeys = V.slice 0 lenSlice originalKeys

                      sndData = V.slice lenSlice (lenKeys - lenSlice) <$> originalData
                      sndKeys = V.slice lenSlice (lenKeys - lenSlice) originalKeys

                      frameUp = RFrameUpdate {_rframeUpdateKeys = fstKeys, _rframeUpdateData = fstData}
                      update = RFrameUpdate {_rframeUpdateKeys = sndKeys, _rframeUpdateData = sndData}

                   frame <- run $ A.fromUpdate frameUp
                      
                   actual <- run $ A.update update frame

                   assert $ actual == expected

-- should have the overlapping change the data
testUpdateOverlap :: Property
testUpdateOverlap = monadicIO $ 
                -- the general idea is to generate an update, then split it into two overlaping parts.
                -- one part will be an update, the other will be the frame. Modify the overlapping part of the frame,
                -- then call A.update and compare with the original
                do original <- pick (arbitrary :: Gen (RFrameUpdate Text Value)) 
                   expected <- run $ A.fromUpdate original

                   let originalKeys = A._rframeUpdateKeys original
                       lenKeys = length originalKeys

                   pre $ not (null originalKeys)

                   lenSlice1 <- pick $ choose (1, lenKeys)
                   startSlice2 <- pick $ choose (0,lenSlice1-1)

                   let
                      lenSlice2 = lenKeys - startSlice2
                      originalData = A._rframeUpdateData original

                      -- data for the base frame
                      fstData = V.slice 0 lenSlice1 <$> originalData
                      fstKeys = V.slice 0 lenSlice1 originalKeys

                      -- the update
                      sndData = V.slice startSlice2 lenSlice2 <$> originalData
                      sndKeys = V.slice startSlice2 lenSlice2 originalKeys

                   -- then we want to modify the "original" data, so we generate random new data for the overlap
                   randPart <- pick $ sequence $ V.replicate (lenSlice1-startSlice2) (arbitrary :: Gen Value)

                   let
                      -- for each row we'll take the part that doesn't overlap, 
                      -- and then concatenate it with the random part
                      randRow = ( flip (V.++) randPart . V.take startSlice2)
                      newFstData = randRow <$> fstData
                      frameUp = RFrameUpdate {_rframeUpdateKeys = fstKeys, _rframeUpdateData = newFstData}
                      update = RFrameUpdate {_rframeUpdateKeys = sndKeys, _rframeUpdateData = sndData}
                   frame <- run $ A.fromUpdate frameUp
                      
                   actual <- run $ A.update update frame
                   --run $ traceIO $ show $ lenKeys * length originalData
                   assert $ actual == expected

testTakeRows :: Property
testTakeRows = monadicIO $ 
                do originalUp <- pick (arbitrary :: Gen (RFrameUpdate Text Value))
                   original <- run $ A.fromUpdate originalUp

                   let originalData = A._rframeUpdateData originalUp
                       lenData = length originalData

                   pre $ not (null originalData)

                   nbRows <- pick $ choose (0, length originalData)

                   let
                      originalKeys = A._rframeUpdateKeys originalUp
                      -- we `take` a random number of rows in the data, then form a new update
                      newData = V.take nbRows originalData
                      newUpdate = RFrameUpdate {_rframeUpdateKeys = originalKeys, _rframeUpdateData = newData}
                   
                   expected <- run $ A.fromUpdate newUpdate
                   
                   -- we remove a row the "conventional" way
                   let
                      actual = A.takeRows nbRows original
                      lenActual = A.numRows actual
              
                   assert $ (actual == expected) && (lenActual == nbRows)

testAddColumn :: Property
testAddColumn = monadicIO $ 
                -- the general idea is to generate an update, then split it, keep the last column as the update, 
                -- transform the other into a frame, and update the second with the first  
                do original <- pick (arbitrary :: Gen (RFrameUpdate Text Value))
                   expected <- run $ A.fromUpdate original

                   let originalKeys = A._rframeUpdateKeys original
                       lenKeys = length originalKeys

                   pre $ not (null originalKeys)

                   let
                      lenSlice = lenKeys -1
                      originalData = A._rframeUpdateData original

                      -- will contain all but the last column
                      fstData = V.slice 0 lenSlice <$> originalData
                      fstKeys = V.slice 0 lenSlice originalKeys

                      -- only last column
                      sndData = V.slice lenSlice (lenKeys - lenSlice) <$> originalData
                      sndKeys = V.slice lenSlice (lenKeys - lenSlice) originalKeys

                      --extracts the the only key, and we need `Vector Value` instead of `Vector (Vector Value)`, 
                      --plus sndData is a vector of singletons
                      key = sndKeys V.! 0
                      column = V.concat $ V.toList sndData

                      beforeUp = RFrameUpdate {_rframeUpdateKeys = fstKeys, _rframeUpdateData = fstData}
                      

                   before <- run $ A.fromUpdate beforeUp
                      
                   actual <- run $ A.addColumn before key column

                   assert $ actual == expected

testOneHot :: Property
testOneHot = monadicIO $ 
                -- the general idea is to generate an update, then split it into two overlapping parts.
                -- one part will be an update, the other will be the frame. Modify the overlapping part of the frame,
                -- then call A.update and compare with the original
                do original <- pick (arbitrary :: Gen (RFrameUpdate Text Value)) 
                   originalFrame <- run $ A.fromUpdate original

                   let originalKeys = A._rframeUpdateKeys original
                       lenKeys = length originalKeys

                   pre $ not (null originalKeys)
                   

                   let
                      keyToTest = V.head originalKeys
                      originalData = A._rframeUpdateData original
                      lenData = length originalData               

                   -- then we want to modify the "original" data, so we generate random new data for the overlap
                   indexTrue <- pick $ choose (0, lenData-1)
                   indexFalse <- pick $ choose (0,lenData-1)

                   let
                       trueValue = (V.head <$> originalData) V.! indexTrue
                       falseValue = (V.head <$> originalData) V.! indexFalse
                       transformedData = (T.pack . show . V.head) <$> originalData
                       newKeys = uniq transformedData
                       newData = V.map (match newKeys trueValue falseValue) transformedData
                                         
                   expected <- run $ A.fromUpdate $ A.RFrameUpdate newKeys newData
                   actual <- run $ A.oneHot (\k v -> (T.pack . show) v) keyToTest trueValue falseValue originalFrame
                   --run $ traceIO $ show $ lenKeys * length originalData
                   run $ traceIO $ "-----------------------\n-----------------------"
                   run $ traceIO $ "Original Update: " ++ show original
                   run $ traceIO $ "-----"
                   run $ traceIO $ "Original data: " ++ show originalData
                   run $ traceIO $ "-----"
                   run $ traceIO $ "Key to test: " ++ show keyToTest
                   run $ traceIO $ "-----"
                   run $ traceIO $ "True/false value: " ++ show trueValue ++ "/" ++ show falseValue
                   run $ traceIO $ "-----"
                   run $ traceIO $ "Expected result: "  ++ show expected 
                   run $ traceIO $ "-----"
                   run $ traceIO $ "From A.oneHot: " ++ show actual
                   assert $ actual == expected

              where 
                match ks yesVal noVal tk = V.map (\k -> if k == tk then yesVal else noVal) ks
                uniq ks = V.reverse (V.fromList newKsR)
                   where
                      acc (hs, uks) k =
                        if HS.member k hs
                        then (hs, uks)
                        else (HS.insert k hs, k:uks)
                      (_, newKsR) = V.foldl acc (HS.empty, []) ks

propTests :: Ts.TestTree
propTests = Ts.testGroup "Test suite"
  [ {-QC.testProperty "Fixture" testFixture,
    QC.testProperty "Row Decode" testRowDecode,
    QC.testProperty "Drop" testDrop,
    QC.testProperty "Keep" testKeep,
    QC.testProperty "Update Empty" testUpdateEmpty,
    QC.testProperty "Update Empty 2" testUpdateEmpty2,
    QC.testProperty "Update Add" testUpdateAdd,
    QC.testProperty "Update Overlap" testUpdateOverlap,
    QC.testProperty "Take Rows" testTakeRows,
    QC.testProperty "Add Column" testAddColumn,-}
    QC.testProperty "One Hot" testOneHot
  ]

main :: IO ()
main = Ts.defaultMain propTests