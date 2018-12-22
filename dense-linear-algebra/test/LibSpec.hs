module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Types as T
import qualified Statistics.Matrix.Algorithms as Alg

import qualified Fixtures as F
import AlgorithmsSpec

spec :: Spec
spec = describe "Q-R Decomposition" $ do
        it "Q x R returns the original matrix" $ do
          qrDecompositionInvariant `shouldBe` True
        it "Matrix Q is orthogonal" $ do
          qrFirstOrthoInvariant `shouldBe` True
        it "Matrix R is triangular" $ do
          qrSecondTriInvariant `shouldBe` True


main :: IO ()
main = do
  hspec spec
  let r = snd $ Alg.qr F.matA
  putStrLn $ show $ M.row r 2




