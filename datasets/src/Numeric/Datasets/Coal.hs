{-# LANGUAGE DeriveGeneric, OverloadedStrings                   #-}
{-# LANGUAGE GADTs, QuasiQuotes, ViewPatterns, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|

Coal data set

Dates of mining disasters, from the `coal` dataset in the R package `boot`.

For further information, see <http://vincentarelbundock.github.io/Rdatasets/doc/boot/coal.html>

-}

module Numeric.Datasets.Coal ( Coal, coal, date ) where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Control.Applicative

data Coal = Coal
  { date :: Double
  } deriving (Show, Read, Generic)

instance FromRecord Coal where
  parseRecord v = Coal <$> v .! 1

coal :: Dataset Coal
coal = let src = URL "http://vincentarelbundock.github.io/Rdatasets/csv/boot/coal.csv"
       in Dataset src Nothing Nothing $ CSVRecord HasHeader defaultDecodeOptions
