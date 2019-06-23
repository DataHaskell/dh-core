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
    case Atto.parseOnly parseArff fileContents of
        Left s -> print $ "Error: " ++ s
        Right (name, attributes, records) -> print (name, attributes, records) 