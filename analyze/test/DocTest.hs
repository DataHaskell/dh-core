{-# OPTIONS_GHC -Wall #-}
module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest [
  "src/Analyze/Frame/Dense/Generic.hs",
  "src/Analyze/Values/Generic.hs"
  ]
