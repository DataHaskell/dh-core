{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{-|

Adult (AKA Census Income) dataset.

UCI ML Repository link <http://archive.ics.uci.edu/ml/datasets/Adult>

-}

module Numeric.Datasets.Adult where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Control.Applicative
import Data.Text (Text, strip)

data WorkClass = Private | SelfEmpNotInc | SelfEmpInc | FederalGov
               | LocalGov | StateGov | WithoutPay | NeverWorked
  deriving (Show, Read, Eq, Generic, Bounded, Enum)

instance FromField WorkClass where
  parseField = parseDashToCamelField


data MaritalStatus = MarriedCivSpouse | Divorced | NeverMarried
                   | Separated | Widowed | MarriedSpouseAbsent | MarriedAFSpouse
  deriving (Show, Read, Eq, Generic, Bounded, Enum)

instance FromField MaritalStatus where
--  parseField "Married-AF-spouse" = pure MarriedAFSpouse
  parseField s = parseDashToCamelField s

data Occupation = TechSupport | CraftRepair | OtherService | Sales | ExecManagerial | ProfSpecialty
                | HandlersCleaners | MachineOpInspct | AdmClerical | FarmingFishing | TransportMoving
                | PrivHouseServ | ProtectiveServ | ArmedForces
  deriving (Show, Read, Eq, Generic, Bounded, Enum)

instance FromField Occupation where
--  parseField "ArmedForces" = pure ArmedForces
  parseField s = parseDashToCamelField s

data Relationship = Wife | OwnChild | Husband | NotInFamily | OtherRelative | Unmarried
  deriving (Show, Read, Eq, Generic, Bounded, Enum)

instance FromField Relationship where
  parseField s = parseDashToCamelField s

data Race = White | AsianPacIslander | AmerIndianEskimo | Other | Black
  deriving (Show, Read, Eq, Generic, Bounded, Enum)

instance FromField Race where
  parseField s = parseDashToCamelField s

data Sex = Female | Male
  deriving (Show, Read, Eq, Generic, Bounded, Enum)

instance FromField Sex where
  parseField s = parseDashToCamelField s

data Income = GT50K | LE50K
  deriving (Show, Read, Eq, Generic, Bounded, Enum)

instance FromField Income where
  parseField " >50K" = pure GT50K
  parseField " <=50K" = pure LE50K
  parseField " >50K." = pure GT50K
  parseField " <=50K." = pure LE50K
  parseField ">50K" = pure GT50K
  parseField "<=50K" = pure LE50K
  parseField _ = fail "unknown income"

data Adult = Adult
  { age :: Int
  , workClass :: Maybe WorkClass
  , finalWeight :: Int
  , education :: Text
  , educationNum :: Int
  , maritalStatus :: MaritalStatus
  , occupation :: Maybe Occupation
  , relationship :: Relationship
  , race :: Race
  , sex :: Sex
  , capitalGain :: Int
  , capitalLoss :: Int
  , hoursPerWeek :: Int
  , nativeCountry :: Text
  , income :: Income
  } deriving (Show, Read, Generic)

instance FromRecord Adult where
  parseRecord v = Adult <$> v .! 0 <*> (v.! 1 <|> return Nothing) <*> v.!2 <*> (strip <$> v.!3)
                        <*> v.!4 <*> v.!5<*> (v.!6 <|> return Nothing) <*> v.!7 <*> v.!8
                        <*> v.!9 <*> v.!10 <*> v.!11 <*> v.!12<*> v.!13<*> v.!14

adult :: Dataset Adult
adult = csvDataset $ URL "http://mlr.cs.umass.edu/ml/machine-learning-databases/adult/adult.data"

adultTestSet :: Dataset Adult
adultTestSet = csvDatasetPreprocess (dropLines 1) $ URL "http://mlr.cs.umass.edu/ml/machine-learning-databases/adult/adult.test"
