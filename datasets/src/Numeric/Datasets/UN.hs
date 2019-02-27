{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{-|

GDP and infant mortality

<http://vincentarelbundock.github.io/Rdatasets/doc/car/UN.html>

-}

module Numeric.Datasets.UN where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Control.Applicative

data GdpMortality = GdpMortality
  { country :: String
  , infantMortality :: Maybe Int
  , gdp :: Maybe Int
  } deriving (Show, Read, Generic)

instance FromNamedRecord GdpMortality where
    parseNamedRecord m = GdpMortality <$>
                         m .: "" <*>
                         (m .: "infant.mortality" <|> return Nothing) <*>
                         (m .: "gdp" <|> return Nothing)

gdpMortalityUN :: Dataset GdpMortality
gdpMortalityUN = csvHdrDataset
   $ URL "http://vincentarelbundock.github.io/Rdatasets/csv/car/UN.csv"
