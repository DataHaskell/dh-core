{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

{- |
Module      :  Numeric.Datasets.Diabetes
Description :  Diabetes dataset created from an ARFF input file
Copyright   :  (c) Arvind Devarajan
License     :  BSD-3-Clause

Maintainer  :  arvindd
Stability   :  experimental
Portability :  portable
-}

module Numeric.Datasets.Diabetes 
            ( DiabetesClass
            , PimaDiabetesEntry
            , pimaDiabetes
            ) where

import Numeric.Datasets    
import Data.FileEmbed    
import qualified Data.ByteString.Lazy as BL (fromStrict, ByteString)
import Numeric.Datasets.Internal.ArffParser

data DiabetesClass = TestedNegative | TestedPositive | UnknownClass deriving (Show)

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

-- | Diabetes dataset, containing a list of Pima Indian diabetes entries
pimaDiabetes :: [PimaDiabetesEntry]
pimaDiabetes = toPimaDiabetes records 
               where records = readArff (BL.fromStrict $(embedFile "datafiles/arff/diabetes.arff"))

-- | Converts each ARFF record into a Pima diabetes entry               
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

        dblval :: Int -> ArffRecord -> Double
        dblval idx r = value (\_->0) idx r

        strval :: Int -> ArffRecord -> BL.ByteString
        strval idx r = value (\_->"") idx r

        diabClass :: Int -> ArffRecord -> DiabetesClass
        diabClass i r = 
            let s = strval i r
            in case s of
                "tested_negative" -> TestedNegative
                "tested_positive" -> TestedPositive
                _                 -> UnknownClass

    in fmap toPD recs