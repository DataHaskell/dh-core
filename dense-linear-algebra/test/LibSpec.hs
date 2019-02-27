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
         --it "Matrix Q is orthogonal for the QR factorization of an invertible matrix" $ do
         -- qrFirstOrthoInvariant F.matD`shouldBe` True
        it "Matrix R is triangular" $ do
          qrSecondTriInvariant F.matB `shouldBe` True


main :: IO ()
main = do
  hspec spec

-- noninv
  let q = fst $ Alg.qr F.matA
  let q' = M.transpose q
  let q_ = makeAbs (M.multiply q q')
 
-- inv
  let r = fst $ Alg.qr F.matB
  let r' = M.transpose r
  let r_ = makeAbs (M.multiply r r')

-- noninv
  let s = fst $ Alg.qr F.matC
  let s' = M.transpose s
  let s_ = makeAbs (M.multiply s s')

-- inv
  let t = fst $ Alg.qr F.matD
  let t' = M.transpose t
  let t_ = makeAbs (M.multiply t t')

  putStrLn "Algorithms.qr: Test 1"
  putStrLn "This is a matrix"
  putStrLn $ show F.matA
  putStrLn "Is matA invertible?"
  putStrLn $ show $ isInvertible F.matA
  putStrLn $ show q_ 

  putStrLn "Algorithms.qr: Test 2"
  putStrLn "This is a matrix"
  putStrLn $ show F.matB
  putStrLn "Is matB invertible?"
  putStrLn $ show $ isInvertible F.matB
  putStrLn $ show r_

  putStrLn "Algorithms.qr: Test 3"
  putStrLn "This is a matrix"
  putStrLn $ show F.matC
  putStrLn "Is matC invertible?"
  putStrLn $ show $ isInvertible F.matC
  putStrLn $ show s_

  putStrLn "Algorithms.qr: Test 4"
  putStrLn "This is a matrix"
  putStrLn $ show F.matD
  putStrLn "Is matD invertible?"
  putStrLn $ show $ isInvertible F.matD
  putStrLn $ show $ t_
  putStrLn $ show $ M.toRowLists t_ 




