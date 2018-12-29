{-# language DeriveGeneric, OverloadedStrings, LambdaCase, DataKinds #-}
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
import Data.FileEmbed
import Data.ByteString.Lazy (fromStrict)
import Network.HTTP.Req ((/:), http, https, Scheme(..))

data TitanicEntry = TitanicEntry {
    tClass :: Class
  , tAge :: Age
  , tSex :: Sex
  , tSurvived :: Bool -- ^ Did the passenger survive ?
  } deriving (Eq, Read, Show, Generic)


instance FromNamedRecord TitanicEntry where
  parseNamedRecord v = TitanicEntry <$>
    (parseClass <$> v .: "Pclass") <*>
    (v .: "Age") <*>
    (parseSex <$> v .: "Sex") <*>
    (parseBool <$> v .: "Survived")     
    

data Class = First | Second | Third | Crew deriving (Eq, Read, Show, Generic, Enum, Bounded)

parseClass :: String -> Class
parseClass = \case
  "1" -> First
  "2" -> Second
  "3" -> Third
  "Crew" -> Crew
  x -> error $ unwords ["Unexpected feature value :", show x]  


newtype Age = Age (Maybe Double) deriving (Eq, Read, Show, Generic)

instance FromField Age where
  parseField s = case s of
    "NA" -> pure $ Age Nothing
    ss -> case runParser (parseField ss :: Parser Double) of
      Left _ -> pure $ Age Nothing
      Right x -> pure $ Age $ Just x  



       

data Sex = Female | Male deriving (Eq, Read, Show, Generic, Enum, Bounded)

parseSex :: String -> Sex
parseSex = \case
  "female" -> Female
  "male" -> Male
  x -> error $ unwords ["Unexpected feature value :", show x]      

parseBool :: String -> Bool
parseBool = \case
  "1" -> True
  "0" -> False
  x -> error $ unwords ["Unexpected feature value :", show x]   


titanic :: Dataset 'Https TitanicEntry
titanic = csvHdrDatasetSep '\t' $ URL $ https "raw.githubusercontent.com" /: "JackStat" /: "6003Data" /: "master" /: "Titanic.txt"

-- https://raw.githubusercontent.com/JackStat/6003Data/master/Titanic.txt





-- "biostat.mc.vanderbilt.edu" /: "wiki" /: "pub" /: "Main" /: "DataSets" /: "titanic.txt"
  
-- http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic.txt

-- https "raw.githubusercontent.com" /: "vincentarelbundock" /: "Rdatasets" /: "master" /: "csv" /: "carData" /: "TitanicSurvival.csv"
  
-- "hofmann.public.iastate.edu" /: "data" /: "titanic.txt"

-- https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/carData/TitanicSurvival.csv




