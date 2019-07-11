{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

{- |
Module      :  Numeric.Datasets.Internal.ArffParser
Description :  Parser for datasets in the Atrribute-Relation File Format (ARFF)
Copyright   :  (c) Arvind Devarajan
License     :  BSD-3-Clause

Maintainer  :  arvindd
Stability   :  experimental
Portability :  portable
-}

module Numeric.Datasets.Diabetes where

import Numeric.Datasets    
import Data.FileEmbed    
import qualified Data.ByteString.Lazy as BL (fromStrict, ByteString)
import Numeric.Datasets.Internal.ArffParser

data DiabetesClass = TestedNegative | TestedPositive | UnknownClass deriving Show

data PimaDiabetesEntry = PimaDiabetesEntry
    { preg          :: !Double
    , plas          :: !Double
    , pres          :: !Double
    , skin          :: !Double
    , insu          :: !Double
    , mass          :: !Double
    , pedi          :: !Double
    , age           :: !Double
    , diabetesClass :: !DiabetesClass
    } deriving (Show)

diabetes = toPimaDiabetes records where
           records = readArff (BL.fromStrict $(embedFile "datafiles/arff/diabetes.arff"))

           toPimaDiabetes :: [ArffRecord] -> [PimaDiabetesEntry]
           toPimaDiabetes recs =   
               let toPD :: ArffRecord -> PimaDiabetesEntry
                   toPD = PimaDiabetesEntry <$> dblval 0 
                                            <*> dblval 1 
                                            <*> dblval 2
                                            <*> dblval 3
                                            <*> dblval 4
                                            <*> dblval 5
                                            <*> dblval 6
                                            <*> dblval 7
                                            <*> diabClass 8

                   diabClass :: Int -> ArffRecord -> DiabetesClass
                   diabClass i r = let s = strval i r
                                   in case s of
                                       "tested_negative" -> TestedNegative
                                       "tested_positive" -> TestedPositive
                                       _                 -> UnknownClass

               in fmap toPD recs