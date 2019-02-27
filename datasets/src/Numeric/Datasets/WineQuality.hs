{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|

Quality of red and white wines based on physicochemical properties

See <http://mlr.cs.umass.edu/ml/datasets/Wine+Quality>

-}

module Numeric.Datasets.WineQuality where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Control.Applicative

data WineQuality = WineQuality
  { fixedAcidity :: Double
  , volatileAcidity :: Double
  , citricAcid :: Double
  , residualSugar :: Double
  , chlorides :: Double
  , freeSulfurDioxide :: Double
  , totalSulfurDioxide :: Double
  , density :: Double
  , pH :: Double
  , sulphates :: Double
  , alcohol :: Double
  , quality :: Int
  } deriving (Show, Read, Generic)

instance FromNamedRecord WineQuality where
    parseNamedRecord m = WineQuality <$>
                         m .: "fixed acidity" <*>
                         m .: "volatile acidity" <*>
                         m .: "citric acid" <*>
                         m .: "residual sugar" <*>
                         m .: "chlorides" <*>
                         m .: "free sulfur dioxide" <*>
                         m .: "total sulfur dioxide" <*>
                         m .: "density" <*>
                         m .: "pH" <*>
                         m .: "sulphates" <*>
                         m .: "alcohol" <*>
                         m .: "quality"

redWineQuality, whiteWineQuality :: Dataset WineQuality
redWineQuality = csvHdrDatasetSep ';'
   $ URL "http://mlr.cs.umass.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"

whiteWineQuality = csvHdrDatasetSep ';'
   $ URL "http://mlr.cs.umass.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
