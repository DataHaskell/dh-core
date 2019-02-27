{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|

Old Faithful Geyser Eruption data

Article: http://web.pdx.edu/~jfreder/M212/oldfaithful.pdf

These data from: <http://www2.stat.duke.edu/courses/Fall02/sta290/datasets/geyser>

For more data, see <http://www.geyserstudy.org/geyser.aspx?pGeyserNo=OLDFAITHFUL>

-}

module Numeric.Datasets.OldFaithful where

import Data.Csv
import Control.Applicative
import Numeric.Datasets



data OldFaithful = OldFaithful
  { waiting :: Double -- ^ waiting time until next eruption
  , duration :: Double -- ^ duration of eruption in minutes
  } deriving Show

instance FromRecord OldFaithful where
  parseRecord v = OldFaithful <$> v .! 2 <*> v.! 1

oldFaithful :: Dataset OldFaithful
oldFaithful
  = let src = URL "https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/datasets/faithful.csv"
    in Dataset src Nothing Nothing $ CSVRecord HasHeader defaultDecodeOptions
