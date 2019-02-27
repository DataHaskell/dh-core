{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|

Data on education in US states

<http://vincentarelbundock.github.io/Rdatasets/doc/car/States.html>

-}

module Numeric.Datasets.States where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Control.Applicative

data StateEdu = StateEdu
  { state :: String
  , region :: String
  , population :: Int
  , satVerbal :: Int
  , satMath :: Int
  , satPercent :: Int
  , dollarSpend :: Double
  , teacherPay :: Int
  } deriving (Show, Read, Generic)

instance FromNamedRecord StateEdu where
    parseNamedRecord m = StateEdu <$>
                         m .: "" <*>
                         m .: "region"  <*>
                         m .: "pop"  <*>
                         m .: "SATV"  <*>
                         m .: "SATM"  <*>
                         m .: "percent"  <*>
                         m .: "dollars"  <*>
                         m .: "pay"

states :: Dataset StateEdu
states = csvHdrDataset
   $ URL "http://vincentarelbundock.github.io/Rdatasets/csv/car/States.csv"
