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
          qrDecompositionInvariant F.matA `shouldBe` True
        --it "Matrix Q is orthogonal" $ do
          --qrFirstOrthoInvariant `shouldBe` True
        it "Matrix R is triangular" $ do
          qrSecondTriInvariant F.matA `shouldBe` True


main :: IO ()
main = do
  hspec spec

  let q = fst $ Alg.qr F.matA
  let q' = M.transpose q
  let q_ = M.multiply q q'
 
  let r = fst $ Alg.qr F.matB
  let r' = M.transpose r
  let r_ = M.multiply r r'

  let s = fst $ Alg.qr F.matC
  let s' = M.transpose s
  let s_ = M.multiply s s'

  let t = fst $ Alg.qr F.matD
  let t' = M.transpose t
  let t_ = M.multiply t t'

  putStrLn "Algorithms.qr: Test 1"
  putStrLn "This is a matrix"
  putStrLn $ show F.matA
  putStrLn "This is q from its QR factorization"
  putStrLn $ show q
  putStrLn "This is q transposed"
  putStrLn $ show q'
  putStrLn "Since matrix Q is supposed to be orthogonal, Q * (Q transposed) should be the identity matrix."
  putStrLn "This is Q * (Q transposed)"
  putStrLn $ show q_

  putStrLn "Algorithms.qr: Test 2"
  putStrLn "This is a matrix"
  putStrLn $ show F.matB
  putStrLn "This is q from its QR factorization"
  putStrLn $ show r
  putStrLn "This is q transposed"
  putStrLn $ show r'
  putStrLn "Since matrix Q is supposed to be orthogonal, Q * (Q transposed) should be the identity matrix."
  putStrLn "This is Q * (Q transposed)"
  putStrLn $ show r_

  putStrLn "Algorithms.qr: Test 3"
  putStrLn "This is a matrix"
  putStrLn $ show F.matC
  putStrLn "This is q from its QR factorization"
  putStrLn $ show s
  putStrLn "This is q transposed"
  putStrLn $ show s'
  putStrLn "Since matrix Q is supposed to be orthogonal, Q * (Q transposed) should be the identity matrix."
  putStrLn "This is Q * (Q transposed)"
  putStrLn $ show s_

  putStrLn "Algorithms.qr: Test 4"
  putStrLn "This is a matrix"
  putStrLn $ show F.matD
  putStrLn "This is q from its QR factorization"
  putStrLn $ show t
  putStrLn "This is q transposed"
  putStrLn $ show t'
  putStrLn "Since matrix Q is supposed to be orthogonal, Q * (Q transposed) should be the identity matrix."
  putStrLn "This is Q * (Q transposed)"
  putStrLn $ show t_




