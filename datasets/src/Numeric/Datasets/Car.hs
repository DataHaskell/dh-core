{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|

Car dataset

UCI ML Repository link <https://archive.ics.uci.edu/ml/datasets/car>

-}

module Numeric.Datasets.Car where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Control.Applicative

data RelScore = Low | Med | High | VeryHigh deriving (Show, Read, Eq, Generic, Bounded, Enum)

instance FromField RelScore where
  parseField "vhigh" = pure VeryHigh
  parseField "high" = pure High
  parseField "med" = pure Med
  parseField "low" = pure Low
  parseField _ = fail "unknown relative score"

data RelSize = Small | Medium | Big deriving (Show, Read, Eq, Generic, Bounded, Enum)

instance FromField RelSize where
  parseField "small" = pure Small
  parseField "med" = pure Medium
  parseField "big" = pure Big
  parseField _ = fail "unknown relative size"

data Acceptability = Unacceptable | Acceptable | Good | VeryGood deriving (Show, Read, Eq, Generic, Bounded, Enum)

instance FromField Acceptability where
  parseField "unacc" = pure Unacceptable
  parseField "acc" = pure Acceptable
  parseField "good" = pure Good
  parseField "vgood" = pure VeryGood
  parseField _ = fail "unknown acceptability"

data Count = N Int | NOrMore Int | More deriving (Show, Read, Eq, Generic)

instance FromField Count where
  parseField "more" = pure More
  parseField "5more" = pure (NOrMore 5)
  parseField "2" = pure (N 2)
  parseField "3" = pure (N 3)
  parseField "4" = pure (N 4)
  parseField _ = fail "unknown count"

data Car = Car
  { buying :: RelScore
  , maintenance :: RelScore
  , doors :: Count
  , persons :: Count
  , luggageBoot :: RelSize
  , safety :: RelScore
  , acceptability:: Acceptability
  } deriving (Show, Read, Generic)

instance FromRecord Car

car :: Dataset Car
car = csvDataset
          $ URL "http://mlr.cs.umass.edu/ml/machine-learning-databases/car/car.data"
