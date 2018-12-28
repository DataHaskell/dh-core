{-# LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds #-}

{-|

Vocabulary and Education

<http://vincentarelbundock.github.io/Rdatasets/doc/car/Vocab.html>

-}

module Numeric.Datasets.Vocabulary where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Network.HTTP.Req ((/:), http, Scheme(..))

data Sex = Female | Male
  deriving (Show, Read, Eq, Generic, Bounded, Enum)

instance FromField Sex where
  parseField = parseReadField

data Vocab = Vocab
  { year :: Integer
  , sex :: Sex
  , education :: Int
  , vocabulary :: Int
  } deriving (Show, Read, Generic)

instance FromNamedRecord Vocab

vocab :: Dataset 'Http Vocab
vocab = csvHdrDataset
   $ URL $ http "vincentarelbundock.github.io" /: "Rdatasets" /: "csv" /: "car" /: "Vocab.csv"
