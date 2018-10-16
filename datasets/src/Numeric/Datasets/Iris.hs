{-# LANGUAGE DeriveGeneric, OverloadedStrings, TemplateHaskell #-}

{-|

The classical Iris dataset, due to R.A. Fisher.

UCI ML Repository link <https://archive.ics.uci.edu/ml/datasets/Iris>

-}

module Numeric.Datasets.Iris where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Data.FileEmbed
import Data.ByteString.Lazy (fromStrict)


data IrisClass = Setosa | Versicolor | Virginica
  deriving (Show, Read, Eq, Ord, Generic, Enum, Bounded)

instance FromField IrisClass where
  parseField "Iris-setosa" = return Setosa
  parseField "Iris-versicolor" = return Versicolor
  parseField "Iris-virginica" = return Virginica
  parseField _ = fail "unknown iris class"

data Iris = Iris
  { sepalLength :: Double
  , sepalWidth :: Double
  , petalLength :: Double
  , petalWidth :: Double
  , irisClass :: IrisClass
  } deriving (Show, Read, Generic)

instance FromRecord Iris

iris :: [Iris]
iris = readDataset csvRecord (fromStrict $(embedFile "datafiles/iris.data"))
