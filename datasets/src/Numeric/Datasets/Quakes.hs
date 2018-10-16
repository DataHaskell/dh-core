{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{-|

Locations of Earthquakes off Fiji

Listed as quakes here: http://vincentarelbundock.github.io/Rdatasets/datasets.html


-}

module Numeric.Datasets.Quakes where

import Numeric.Datasets

import Data.Csv
import GHC.Generics

data Quake = Quake
  { lat :: Double
  , long :: Double
  , depth :: Double
  , mag :: Double
  , stations :: Int
  } deriving (Show, Read, Generic)

instance FromNamedRecord Quake

quakes :: Dataset Quake
quakes = csvHdrDataset
   $ URL "http://vincentarelbundock.github.io/Rdatasets/csv/datasets/quakes.csv"
