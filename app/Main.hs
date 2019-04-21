{-# LANGUAGE OverloadedStrings #-}

module Main where

import ArffParser
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString.Lazy as Atto  
import System.Environment  

main :: IO ()
main = do
    args <- getArgs
    let arffFile = "E:/Workspace/datahaskell/arffparser/data/" ++ head args ++ ".arff"
    fileContents <- B.readFile arffFile
    print $ Atto.parseOnly parseArff fileContents