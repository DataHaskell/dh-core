{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{-|

Wine Data set

UCI ML Repository link <http://mlr.cs.umass.edu/ml/datasets/Wine>

-}

module Numeric.Datasets.Wine where

import Numeric.Datasets

import Data.Csv
import GHC.Generics

data Wine = Wine
  { wineClass :: Int
  , alcohol :: Double
  , malicAcid :: Double
  , ash :: Double
  , ashAlcalinity :: Double
  , magnesium :: Double
  , totalPhenols :: Double
  , flavanoids :: Double
  , nonflavanoidPhenols :: Double
  , proanthocyanins :: Double
  , colorIntensity :: Double
  , hue :: Double
  , dilutedOD280toOD315 :: Double
  , proline :: Int
  } deriving (Show, Read, Generic)

instance FromRecord Wine

wine :: Dataset Wine
wine = csvDatasetPreprocess
            fixAmericanDecimals
            $ URL "http://mlr.cs.umass.edu/ml/machine-learning-databases/wine/wine.data"
