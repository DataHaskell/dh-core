datasets: data sets for statistics and machine learning, in Haskell
=====

[![Hackage](https://img.shields.io/hackage/v/datasets.svg)](https://hackage.haskell.org/package/datasets) [![Build Status](https://secure.travis-ci.org/glutamate/datasets.svg)](http://travis-ci.org/glutamate/datasets)

This library provides easy access in Haskell to a series of data sets
for Statistics and Machine learning.

Most of these datasets come from the [UCI Machine Learning Reposity](http://archive.ics.uci.edu/ml/)
([Mirror](http://mlr.cs.umass.edu/ml/))

## Usage

```haskell

import Numeric.Datasets (getDataset)
import Numeric.Datasets.Iris (iris)
import Numeric.Datasets.Abalone (abalone)

main = do

  -- The Iris data set is embedded
  print (length iris)
  print (head iris)

  -- The Abalone dataset is fetched
  abas <- getDataset abalone
  print (length abas)
  print (head abas)

```
