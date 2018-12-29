{-# LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds #-}

{-|

Wine Data set

UCI ML Repository link <http://mlr.cs.umass.edu/ml/datasets/Wine>

-}

module Numeric.Datasets.Wine where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Network.HTTP.Req ((/:), http, Scheme(..))

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

wine :: Dataset 'Http Wine
wine = withPreprocess fixAmericanDecimals $
          csvDataset $ URL $ umassMLDB /: "wine" /: "wine.data"
