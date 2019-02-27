{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{-|

Abalone data set

UCI ML Repository link <https://archive.ics.uci.edu/ml/datasets/abalone>

-}

module Numeric.Datasets.Abalone where

import Numeric.Datasets

import Data.Csv
import GHC.Generics

data Sex = M | F | I
  deriving (Show, Read, Eq, Generic, Bounded, Enum)

instance FromField Sex where
  parseField = parseReadField

data Abalone = Abalone
  { sex :: Sex
  , abaloneLength :: Double
  , diameter :: Double
  , height :: Double
  , wholeWeight :: Double
  , shuckedWeight :: Double
  , visceraWeight :: Double
  , shellWeight :: Double
  , rings :: Int
  } deriving (Show, Read, Generic)

instance FromRecord Abalone

abalone :: Dataset Abalone
abalone = csvDataset $ URL "http://mlr.cs.umass.edu/ml/machine-learning-databases/abalone/abalone.data"
