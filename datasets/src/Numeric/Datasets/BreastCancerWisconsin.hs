{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{-|

Breast Cancer Wisconsin (Diagnostic) Data Set

Repository link: <http://mlr.cs.umass.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29>

-}

module Numeric.Datasets.BreastCancerWisconsin where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Control.Applicative


data Diagnosis = Malignant | Benign deriving (Show, Read, Eq, Generic, Bounded, Enum)

data Prognosis = Recurrent | Nonrecurrent deriving (Show, Read, Eq, Generic, Bounded, Enum)

intToDiagnosis :: Int -> Diagnosis
intToDiagnosis 2 = Benign
intToDiagnosis 4 = Malignant
intToDiagnosis _ = error "unknown diagnosis code"

data BreastCancerEntry = BreastCancerEntry
 { sampleCodeNumber :: Int
 , clumpThickness :: Int
 , uniformityCellSize :: Int
 , uniformityCellShape :: Int
 , marginalAdhesion :: Int
 , singleEpithelialCellSize :: Int
 , bareNuclei :: Maybe Int
 , blandChromatin :: Int
 , normalNucleoli :: Int
 , mitosis :: Int
 , sampleClass :: Diagnosis
 } deriving (Show, Read, Generic)

instance FromRecord BreastCancerEntry where
  parseRecord v = BreastCancerEntry <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5 <*> (v .! 6 <|> return Nothing) <*> v .! 7  <*> v .! 8  <*> v .! 9  <*> (intToDiagnosis <$> v .! 10)

breastCancerDatabase :: Dataset BreastCancerEntry
breastCancerDatabase = csvDataset
   $ URL "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"

data DiagnosticBreastCancer = DiagnosticBreastCancer
  { diagnosticID :: Int
  , diagnosis :: Diagnosis
  , diagnosticCells :: CellFeatures
  } deriving (Show, Read, Generic)

data PrognosticBreastCancer = PrognosticBreastCancer
  { prognosticID :: Int
  , prognosis :: Prognosis
  , prognosticCells :: CellFeatures
  } deriving (Show, Read, Generic)

data CellFeatures = CellFeatures
  { radius :: Double
  , perimeter :: Double
  , area :: Double
  , smoothness :: Double
  , compactness :: Double
  , concavity :: Double
  , concavePoints :: Double
  , symmetry :: Double
  , fractalDimension :: Double
  } deriving (Show, Read, Generic)

charToDiagnosis :: String -> Diagnosis
charToDiagnosis "M" = Malignant
charToDiagnosis "B" = Benign
charToDiagnosis _ = error "unknown diagnosis"

charToPrognosis :: String -> Prognosis
charToPrognosis "N" = Nonrecurrent
charToPrognosis "R" = Recurrent
charToPrognosis _ = error "unknown diagnosis"

instance FromRecord DiagnosticBreastCancer where
  parseRecord v = DiagnosticBreastCancer <$> v .! 0 <*> (charToDiagnosis <$> v .! 1) <*> parseRecord v

instance FromRecord PrognosticBreastCancer where
  parseRecord v = PrognosticBreastCancer <$> v .! 0 <*> (charToPrognosis <$> v .! 1) <*> parseRecord v

instance FromRecord CellFeatures where
  parseRecord v = CellFeatures <$> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5 <*> v .! 6  <*> v .! 7  <*> v .! 8  <*> v .! 9  <*> v .! 10

diagnosticBreastCancer :: Dataset DiagnosticBreastCancer
diagnosticBreastCancer = csvDataset
   $ URL "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"

prognosticBreastCancer :: Dataset PrognosticBreastCancer
prognosticBreastCancer = csvDataset
   $ URL "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wpbc.data"
