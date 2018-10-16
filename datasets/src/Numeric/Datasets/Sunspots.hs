{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|

Monthly sunspots from 1749

Listed as sunspot.month here: http://vincentarelbundock.github.io/Rdatasets/datasets.html

See <http://vincentarelbundock.github.io/Rdatasets/doc/datasets/sunspot.month.html>

-}

module Numeric.Datasets.Sunspots where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Control.Applicative

data Sunspot = Sunspot
  { time :: Double
  , sunspotMonth :: Double
  } deriving (Show, Read, Generic)

instance FromNamedRecord Sunspot where
    parseNamedRecord m = Sunspot <$>
                         m .: "time" <*>
                         m .: "sunspot.month"

sunspots :: Dataset Sunspot
sunspots = csvHdrDataset
   $ URL "http://vincentarelbundock.github.io/Rdatasets/csv/datasets/sunspot.month.csv"
