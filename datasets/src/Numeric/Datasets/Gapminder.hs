{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|

Gapminder dataset - Life expectancy, GDP, population every five years per country

Source: <https://raw.githubusercontent.com/plotly/datasets/master/gapminderDataFiveYear.csv>

More information: https://cran.r-project.org/web/packages/gapminder/gapminder.pdf

-}

module Numeric.Datasets.Gapminder where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Control.Applicative
import Data.Text (Text)

data Gapminder = Gapminder
  { country :: Text
  , year :: Int
  , pop :: Integer
  , continent :: Text
  , lifeExp :: Double
  , gdpPercap :: Double
  } deriving (Show, Read, Generic)

instance FromNamedRecord Gapminder where
    parseNamedRecord m = Gapminder <$>
                         m .: "country" <*>
                         m .: "year" <*>
                         (roundIt <$> m .: "pop") <*>
                         m .: "continent" <*>
                         m .: "lifeExp" <*>
                         m .: "gdpPercap"
          where roundIt :: Double -> Integer
                roundIt = round

gapminder :: Dataset Gapminder
gapminder = csvHdrDataset
   $ URL "https://raw.githubusercontent.com/plotly/datasets/master/gapminderDataFiveYear.csv"
