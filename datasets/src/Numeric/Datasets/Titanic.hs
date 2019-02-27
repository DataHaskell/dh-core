{-# language DeriveGeneric, OverloadedStrings, LambdaCase #-}
{-|

Titanic Data Set. For each person on board the fatal maiden voyage of the ocean liner SS Titanic, this dataset records Sex, Age (child/adult), Class (Crew, 1st, 2nd, 3rd Class) and whether or not the person survived.

``The Titanic survival data seem to become to categorical data analysis what Fisher's Iris data are to discriminant analysis.'' - Buja A.: A word from the editor of JCGS. Statistical Computing & Graphics Newsletter 10 (1), pp.32-33, 1999.

Retrieved from: <http://www.public.iastate.edu/~hofmann/data/titanic.txt>

Header:

Class	Age	Sex	Survived

Example rows :

Second	Child	Female	Yes
Third	Adult	Male	Yes

-}
module Numeric.Datasets.Titanic (titanic, TitanicEntry(..), Class(..), Age(..), Sex(..))where

import Numeric.Datasets

import Data.Csv
import GHC.Generics

data TitanicEntry = TitanicEntry {
    tClass :: Class
  , tAge :: Age
  , tSex :: Sex
  , tSurvived :: Bool -- ^ Did the passenger survive ?
  } deriving (Eq, Read, Show, Generic)


instance FromNamedRecord TitanicEntry where
  parseNamedRecord v = TitanicEntry <$>
    (parseClass <$> v .: "Class") <*>
    (parseAge <$> v .: "Age") <*>
    (parseSex <$> v .: "Sex") <*>
    (parseBool <$> v .: "Survived")     
    

data Class = First | Second | Third | Crew deriving (Eq, Read, Show, Generic, Enum, Bounded)

parseClass :: String -> Class
parseClass = \case
  "First" -> First
  "Second" -> Second
  "Third" -> Third
  "Crew" -> Crew
  x -> error $ unwords ["Unexpected feature value :", show x]  

data Age = Child | Adult deriving (Eq, Read, Show, Generic, Enum, Bounded)
parseAge :: String -> Age
parseAge = \case
  "Child" -> Child
  "Adult" -> Adult
  x -> error $ unwords ["Unexpected feature value :", show x]    

data Sex = Female | Male deriving (Eq, Read, Show, Generic, Enum, Bounded)
parseSex :: String -> Sex
parseSex = \case
  "Female" -> Female
  "Male" -> Male
  x -> error $ unwords ["Unexpected feature value :", show x]      

parseBool :: String -> Bool
parseBool = \case
  "Yes" -> True
  "No" -> False
  x -> error $ unwords ["Unexpected feature value :", show x]   
  

titanic :: Dataset TitanicEntry
titanic = csvHdrDatasetSep '\t' $ URL "http://www.public.iastate.edu/~hofmann/data/titanic.txt"
