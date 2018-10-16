{-# LANGUAGE DeriveGeneric, OverloadedStrings, TemplateHaskell #-}

{-|

Florence Nightingale's count of injuries in the Crimean War, used for her rose plots

Data from <https://github.com/datasets-io/nightingales-rose>

The embedded dataset is Copyright (c) 2015 The Compute.io Authors.

-}

module Numeric.Datasets.Nightingale where

import Numeric.Datasets
import Data.FileEmbed
import Data.ByteString.Lazy (fromStrict)
import Data.Aeson hiding (parseJSON)
import Data.Time (UTCTime)
import GHC.Generics


data Nightingale = Nightingale
  { date :: UTCTime
  , army_size :: Int
  , disease :: Int
  , wounds :: Int
  , other :: Int
  } deriving (Show, Read, Generic)

instance FromJSON Nightingale

nightingale :: [Nightingale]
nightingale = readDataset JSON $ fromStrict $(embedFile "datafiles/nightingale.json")
