module Main where

import qualified Analyze as A
import           Test.Tasty
import           Test.Tasty.HUnit 
import           Test.Tasty.Quickcheck
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)

import MoneyFixtures
import MoneySpent 

testRemoveLegalFees :: TestTree
testRemoveLegalFees = testCase "removeLegalFees" $ do
    table <- decodeWithHeader $ encodeUtf8 exampleUnitsCsv
    noFees <- decodeWithHeader noLegalFees
    removeLegalFees table ?@= noFees

testGroupBy :: TestTree
testGroupBy = testCase "groupBy (person)" $ do
    grouped <- mapM decodeWithHeader personsGrouped
    table <- decodeWithHeader $ encodeUtf8 exampleUnitsCsv
    people <- groupBy table (T.pack "person")  
    grouped ?@= people
    
testWithPriceCol :: TestTree 
testWithPriceCol = testCase "addPriceCol" $ do 
    table <- decodeWithHeader $ encodeUtf8 exampleUnitsCsv
    prices <- decodeWithHeader $ encodeUtf8 examplePriceCsv
    examplePC <- mapM decodeWithHeader priceCol 
    splitItems <- groupBy table (T.pack "item-bought")
    priceCol <- addPriceCol splitItems prices
    priceCol ?@= examplePC
    
testDatesFiltered :: TestTree
testDatesFiltered = testCase "datesFiltered" $ do 
    table <- decodeWithHeader $ encodeUtf8 exampleUnitsCsv
    filtered <- decodeWithHeader datesFiltered
    filterDates table ?@= filtered

testAccSum :: TestTree
testAccSum = testCase "accumSumCol" $ do
    table <- decodeWithHeader $ head personsGrouped
    accumSumCol table ?@= accumSum

testMerge :: TestTree 
testMerge = testCase "merge" $ do 
    tables <- mapM decodeWithHeader personsGrouped
    full <- encodeWithHeader $ encodeUtf8 exampleUnitsCsv
    merge tables ?@= full 

testMean :: TestTree
testMean = testCase "mean" $ do
    pc <- mapM decodeWithHeader withPriceCol
    table <- merge pc 
    mean table ?@= totalMean

tests :: TestTree
tests = [
    testGroupBy,    
    testWithPriceCol,    
    testDatesFiltered,
    testAccSum,
    testMerge,
    testMean
    ]

main :: IO ()
main = defaultMain tests
