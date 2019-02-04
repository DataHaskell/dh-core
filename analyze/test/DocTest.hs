{-# OPTIONS_GHC -Wall #-}
module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest [
  "src/Analyze/RFrame/Generic.hs",
  "src/Analyze/Values/Generic.hs"
  ]
