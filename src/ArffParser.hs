{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- |
Module      :  ArffParser
Description :  Parser for datasets in the Atrribute-Relation File Format (ARFF)
Copyright   :  (c) Arvind Devarajan, datahaskell
License     :  MIT

Maintainer  :  arvindd
Stability   :  unstable
Portability :  portable
-}

module ArffParser where
    -- ( parseArff
    -- , DataType
    -- , Attribute
    -- , Content
    -- ) where

import Control.Applicative        
import qualified Data.Char as C
import Debug.Trace
import Data.Attoparsec.ByteString.Lazy
    hiding (take, takeWhile) 
import Data.Attoparsec.ByteString.Char8 as AttoC 
    hiding (skipWhile, take, takeWhile)

-- | Data types of attributes
data DataType = Numeric 
              | Integer
              | Real
              | Class 
              | String
              | Date 
              | Relational
        deriving (Show)

-- | Each attribute in the file        
data Attribute = Attribute 
    { name :: !String
    , dataType :: !DataType  
    } deriving (Show)       

-- | Complete content of the ARFF file
data Content = Content
    { relationName :: !String
    , attributeNames :: ![Attribute]
    , dataSamples :: ![String]
    } deriving (Show)

-- | Parse the ARFF file and fill contents in `Content`    
parseArff :: Parser Content
parseArff = do
    skipMany comment >> spaces
    rel <- relation
    skipMany comment >> spaces
    atts <- readAttributes
    skipMany comment >> spaces
    -- dats <- readDataSamples
    -- skipMany comment >> spaces
    return $ Content rel atts ["hello"]

----------------------- All parsers --------------------------

spaces :: Parser ()
spaces = skipWhile (\x -> isSpace_w8 x || isEndOfLine x)

comment :: Parser String
comment = string "%" >> manyTill anyChar endOfLine

relation :: Parser String
relation = stringCI "@relation" >> spaces >> manyTill anyChar endOfLine

attribute :: Parser String
attribute = stringCI "@attribute" >> spaces >> manyTill anyChar endOfLine

readAttributes :: Parser [Attribute]
readAttributes = do
    attStrs <- many' attribute
    return $ makeAttribute <$> attStrs
  where   
    makeAttribute :: String -> Attribute
    makeAttribute s = Attribute (attname s) (dattype s) where 
        attname :: String -> String
        attname s
            | head s == '"' = takeWhile (\x -> x /= '"') $ tail s
            | otherwise     = takeWhile (\x -> (x /= ' ') && (x /= '\t')) s
        dattype :: String -> DataType            
        dattype s = case (strToLower $ dtype s) of
            "numeric"    -> Numeric            
            "integer"    -> Integer
            "real"       -> Real
            "string"     -> String
            "date"       -> Date
            "relational" -> Relational
            _            -> Numeric
          where
            dtype s = takeWhile (\x -> (x /= '\r') || (x /= '\n')) 
                        $ drop (length $ attname s) s
    
strToLower :: String -> String
strToLower s = [C.toLower x | x <- s, not $ (C.isSpace x) ]

readDataSamples :: Parser [String]
readDataSamples = undefined