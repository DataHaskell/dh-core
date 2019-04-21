{-# LANGUAGE OverloadedStrings #-}

module Main where

import ArffParser
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString.Lazy as Atto    

main :: IO ()
main = do
    fileContents <- B.readFile "E:/Workspace/datahaskell/arffparser/data/contact-lens.arff"
    print $ Atto.parseOnly parseArff fileContents