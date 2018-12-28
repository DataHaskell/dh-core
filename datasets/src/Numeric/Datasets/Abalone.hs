{-# LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds #-}

{-|

Abalone data set

UCI ML Repository link <https://archive.ics.uci.edu/ml/datasets/abalone>

-}

module Numeric.Datasets.Abalone where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Network.HTTP.Req ((/:), http, Scheme(..))

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

abalone :: Dataset 'Http Abalone
abalone = csvDataset $ URL $ umassMLDB /: "abalone" /: "abalone.data"

