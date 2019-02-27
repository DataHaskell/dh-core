{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{-|

Mauna Loa CO2 time-series


Listed as co2 here: http://vincentarelbundock.github.io/Rdatasets/datasets.html

See <http://vincentarelbundock.github.io/Rdatasets/doc/datasets/co2.html>

-}

module Numeric.Datasets.CO2 where

import Numeric.Datasets

import Data.Csv
import GHC.Generics

data CO2 = CO2
  { time :: Double
  , co2 :: Double
  } deriving (Show, Read, Generic)

instance FromNamedRecord CO2

maunaLoaCO2 :: Dataset CO2
maunaLoaCO2 = csvHdrDataset
   $ URL "http://vincentarelbundock.github.io/Rdatasets/csv/datasets/co2.csv"
