module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest [
  "src/Core/Data/Frame/Generic.hs",
  "src/Core/Data/Frame.hs"
  ]
