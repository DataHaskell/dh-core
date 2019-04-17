module Main where

import qualified PropTests             as P
import qualified UnitTests             as U
import qualified Test.Tasty            as Ts

groupTest :: Ts.TestTree
groupTest = Ts.testGroup "Analyze tests" [P.propTests, U.tests]

main :: IO ()
main = do
  Ts.defaultMain groupTest