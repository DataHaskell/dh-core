module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Types as T
import qualified Statistics.Matrix.Algorithms as Alg

import qualified Fixtures as F
import AlgorithmsSpec
import Utils

spec :: Spec
spec = describe "Q-R Decomposition" $ do
        it "Q x R returns the original matrix" $ do
          qrDecompositionInvariant F.matB `shouldBe` True
        it "Matrix Q is orthogonal for the QR factorization of an invertible matrix" $ do
          qrFirstOrthoInvariant F.matD`shouldBe` True
        it "Matrix R is triangular" $ do
          qrSecondTriInvariant F.matB `shouldBe` True


main :: IO ()
main = do
  hspec spec

---------------- gigantic manual testing thing that I wrote ------------------------------

-- noninv
--  let q = fst $ Alg.qr F.matA
--  let q' = M.transpose q
--  let q_ = makeAbs (M.multiply q q')

-- inv
--  let r = fst $ Alg.qr F.matB
--  let r' = M.transpose r
--  let r_ = makeAbs (M.multiply r r')

-- noninv
--  let s = fst $ Alg.qr F.matC
--  let s' = M.transpose s
--  let s_ = makeAbs (M.multiply s s')

-- inv
--  let t = fst $ Alg.qr F.matD
--  let t' = M.transpose t
--  let t_ = makeAbs (M.multiply t t')

--  putStrLn "Algorithms.qr: Test 1"
--  putStrLn $ show $ isEqual 8 q_ F.matId

--  putStrLn "Algorithms.qr: Test 2"
--  putStrLn $ show $ isEqual 8 r_ F.matId

--  putStrLn "Algorithms.qr: Test 3"
--  putStrLn $ show $ isEqual 8 s_ F.matId

--  putStrLn "Algorithms.qr: Test 4"
--  putStrLn $ show $ isEqual 8 t_ F.matId

--------------------------------------------------------------------------------------------


