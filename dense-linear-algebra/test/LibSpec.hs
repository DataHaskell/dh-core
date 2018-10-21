module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Statistics.Matrix as StMat
import qualified Statistics.Matrix.Types as StTypes

spec :: Spec
spec = describe "Testing the tests..." $ do
        it "Seeing if test library works" $ do
            head [1, 2] `shouldBe` (1 :: Int)

mat :: StTypes.Matrix
mat = StMat.fromList 2 2 [1,2,3,4]

main :: IO ()
main = do
  hspec spec
  putStrLn $ show mat
  



