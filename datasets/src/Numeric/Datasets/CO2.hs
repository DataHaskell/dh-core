{-# LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds #-}

{-|

Mauna Loa CO2 time-series


Listed as co2 here: http://vincentarelbundock.github.io/Rdatasets/datasets.html

See <http://vincentarelbundock.github.io/Rdatasets/doc/datasets/co2.html>

-}

module Numeric.Datasets.CO2 where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Network.HTTP.Req ((/:), https, Scheme(..))

data CO2 = CO2
  { time :: Double
  , value :: Double
  } deriving (Show, Read, Generic)

instance FromNamedRecord CO2

maunaLoaCO2 :: Dataset 'Https CO2
maunaLoaCO2 = csvHdrDataset
   $ URL $ https "raw.githubusercontent.com" /: "vincentarelbundock" /: "Rdatasets" /: "master" /: "csv" /: "datasets" /: "CO2.csv"

