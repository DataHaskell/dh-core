{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import qualified Analyze                  as A
import           Analyze.Common           ((<&>))
import           Control.Monad.Catch
import qualified Data.HashMap.Strict      as HM
import qualified Data.HashSet             as HS
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Vector              (Vector)
import qualified Data.Vector              as V
import           Fixtures
import           Generation
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

-- Boilerplate

propertyIO :: Assertion -> Property
propertyIO action = ioProperty tester
  where
    tester :: IO P.Result
    tester = catch (action >> return P.succeeded) handler
    handler (HUnitFailure err) = return P.failed { P.reason = err }

testPropertyIO :: TestName -> Gen a -> (a -> Assertion) -> TestTree
testPropertyIO name g t = testProperty name (propertyIO . t <$> g)

-- Aux

getUpdateFixture :: Text -> IO (A.RFrameUpdate Text A.Value)
getUpdateFixture name =
  case HM.lookup name fixtures of
    Just u  -> return u
    Nothing -> error ("fixture not found: " ++ T.unpack name)


getFrameFixture :: Text -> IO (A.RFrame Text A.Value)
getFrameFixture name = A.fromUpdate =<< getUpdateFixture name

-- Tests

testFixture :: TestTree
testFixture = testCase "fixture" $ do
  frame <- getFrameFixture "full"
  A._rframeKeys frame @?= exampleHeader
  A.numRows frame @?= 2
  A.numCols frame @?= 3

testRowDecode :: TestTree
testRowDecode = testCase "rowDecode" $ do
  frame <- getFrameFixture "full"
  let decoder = A.requireWhere "score" A.floating <&> (*2)
  result <- sequenceA =<< A.decode decoder frame
  V.fromList [10.0, 6.0] @?= result

testDrop :: TestTree
testDrop = testCase "drop" $ do
  original <- getFrameFixture "full"
  expected <- getFrameFixture "noName"
  A.numCols original @?= 3
  A.numCols expected @?= 2
  let names = HS.singleton "name"
  let actual = A.dropCols (`HS.member` names) original
  A._rframeKeys actual @?= A._rframeKeys expected

testKeep :: TestTree
testKeep = testCase "keep" $ do
  original <- getFrameFixture "full"
  expected <- getFrameFixture "noName"
  A.numCols original @?= 3
  A.numCols expected @?= 2
  let names = HS.fromList ["id", "score"]
  let actual = A.keepCols (`HS.member` names) original
  A._rframeKeys actual @?= A._rframeKeys expected

testUpdateEmpty :: TestTree
testUpdateEmpty = testCase "update empty" $ do
  update <- getUpdateFixture "full"
  empty <- A.fromUpdate =<< getUpdateFixture "empty"
  expected <- A.fromUpdate update
  actual <- A.update update empty
  actual @?= expected

testUpdateEmpty2 :: TestTree
testUpdateEmpty2 = testCase "update empty 2" $ do
  frame <- getFrameFixture "full"
  update <- getUpdateFixture "empty"
  actual <- A.update update frame
  actual @?= frame

testUpdateAdd :: TestTree
testUpdateAdd = testCase "update add" $ do
  frame <- getFrameFixture "full"
  update <- getUpdateFixture "color"
  expected <- getFrameFixture "fullColor"
  actual <- A.update update frame
  actual @?= expected

testUpdateOverlap :: TestTree
testUpdateOverlap = testCase "update overlap" $ do
  frame <- getFrameFixture "full"
  update <- getUpdateFixture "overlap"
  expected <- getFrameFixture "fullOverlap"
  actual <- A.update update frame
  actual @?= expected

testTakeRows :: TestTree
testTakeRows = testCase "takeRows" $ do
  before <- getFrameFixture "color"
  A.numRows before @?= 2
  expected <- getFrameFixture "colorOne"
  A.numRows expected @?= 1
  let actual = A.takeRows 1 before
  --A.numRows actual @?= 1
  actual @?= expected

testAddColumn :: TestTree
testAddColumn = testCase "add column" $ do
  before <- getFrameFixture "color"
  A.numCols before @?= 1
  expected <- getFrameFixture "colorSpanish"
  A.numCols expected @?= 2
  actual <- A.addColumn before "spanishColor" (V.fromList [ A.ValueText "lila", A.ValueText "naranja"])
  --A.numRows actual @?= 1
  actual @?= expected

testOneHot :: TestTree
testOneHot = testCase "one hot" $ do
  color <- getFrameFixture "color"
  colorHot <- getFrameFixture "colorHot"
  actual <- A.oneHot (\k (A.ValueText v) -> k <> "/" <> v) "color" (A.ValueBool True) (A.ValueBool False) color
  actual @?= colorHot

-- Runner

tests :: TestTree
tests = testGroup "Tests"
  [ testFixture
  , testRowDecode
  , testDrop
  , testKeep
  , testUpdateEmpty
  , testUpdateEmpty2
  , testUpdateAdd
  , testUpdateOverlap
  , testTakeRows
  , testAddColumn
  , testOneHot
  ]

main :: IO ()
main = defaultMain tests
