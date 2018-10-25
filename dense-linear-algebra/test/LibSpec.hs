module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Types as T
import qualified Statistics.Matrix.Algorithms as Alg

import qualified Fixtures as F

spec :: Spec
spec = describe "Q-R Decomposition" $ do
        it "Q*R returns the original matrix" $ do
            qrDecompositionInvariant `shouldBe` True

qrDecompositionInvariant :: Bool
qrDecompositionInvariant =
  (M.multiply (fst res) (snd res)) == F.matA
  where
    res = Alg.qr F.matA

matC :: T.Matrix
matC = snd res where
  res = Alg.qr F.matA

main :: IO ()
main = do
  hspec spec
  putStrLn $ show matC




