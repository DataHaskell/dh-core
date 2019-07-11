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
import Data.ByteString.Lazy (fromStrict)
import Numeric.Datasets.Internal.ArffParser

data DiabetesClass = TestedNegative | TestedPositive deriving Show

data PimaDiabetes = PimaDiabetes
    { preg :: Double
    , plas :: Double
    , pres :: Double
    , skin :: Double
    , insu :: Double
    , mass :: Double
    , pedi :: Double
    , age :: Double
    , diabetesClass :: DiabetesClass
    } deriving (Show)

diabetesRA :: ReadAs ArffRecord
diabetesRA = MultiRecordParsable arffRecords

diabetesDS :: [ArffRecord]
diabetesDS = readDataset diabetesRA (fromStrict $(embedFile "datafiles/arff/diabetes.arff"))

