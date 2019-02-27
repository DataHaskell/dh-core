{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|

BostonHousing Data set

scikit-learn calls this "boston" and UCI calls it "Housing"

UCI ML Repository link <http://mlr.cs.umass.edu/ml/datasets/housing>

-}

module Numeric.Datasets.BostonHousing where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Control.Applicative


data BostonHousing = BostonHousing
  { crimeRate :: Double
  , zoned :: Double
  , industrial :: Double
  , charlesRiver :: Bool
  , nitricOxides :: Double
  , rooms :: Double
  , age :: Double
  , distance :: Double
  , radialHwy :: Double
  , tax :: Double
  , ptRatio :: Double
  , b :: Double
  , lowerStatus :: Double
  , medianValue :: Double
  } deriving (Show, Read, Generic)

instance FromRecord BostonHousing where
  parseRecord v = BostonHousing <$>
     v .! 0 <*>
     v .! 1 <*>
     v .! 2 <*>
     (intToBool <$> v .! 3) <*>
     v .! 4 <*>
     v .! 5 <*>
     v .! 6 <*>
     v .! 7 <*>
     v .! 8 <*>
     v .! 9 <*>
     v .! 10 <*>
     v .! 11 <*>
     v .! 12 <*>
     v .! 13
       where intToBool :: Int -> Bool
             intToBool 0 = False
             intToBool 1 = True
             intToBool _ = error "intToBool"
bostonHousing :: Dataset BostonHousing
bostonHousing = csvDatasetPreprocess
            fixedWidthToCSV
            $ URL "http://mlr.cs.umass.edu/ml/machine-learning-databases/housing/housing.data"
