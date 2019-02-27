{-# LANGUAGE DeriveGeneric, OverloadedStrings, TemplateHaskell #-}

{-|

Michelson's speed of light dataset - five repeated measurements of the speed of light.

Data from <https://github.com/datasets-io/michelson-speed-of-light>

The embedded dataset is Copyright (c) 2015 The Compute.io Authors.

-}

module Numeric.Datasets.Michelson where

import Numeric.Datasets
import Data.FileEmbed
import Data.ByteString.Lazy (fromStrict)


michelson :: [[Double]]
michelson = readDataset JSON (fromStrict $(embedFile "datafiles/michelson.json"))
