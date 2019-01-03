module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Spec" $ 
    it "works" $ do
      41 + 1 `shouldBe` 42
