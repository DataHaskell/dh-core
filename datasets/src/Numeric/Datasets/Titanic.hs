{-# language DeriveGeneric, OverloadedStrings, LambdaCase, DataKinds #-}
{-|

Titanic Data Set. For each person on board the fatal maiden voyage of the ocean liner SS Titanic, this dataset records Sex, Age (child/adult), Class (Crew, 1st, 2nd, 3rd Class) and whether or not the person survived.

``The Titanic survival data seem to become to categorical data analysis what Fisher's Iris data are to discriminant analysis.'' - Buja A.: A word from the editor of JCGS. Statistical Computing & Graphics Newsletter 10 (1), pp.32-33, 1999.

Retrieved from: <https://raw.githubusercontent.com/JackStat/6003Data/master/Titanic.txt>. A copy of the dataset can be found in datasets/titanic2_full.tsv .

Header:

"PassengerId"	"Survived"	"Pclass"	"Name"	"Sex"	"Age"	"SibSp"	"Parch"	"Ticket"	"Fare"	"Cabin"	"Embarked"

Example rows :

10	1	2	"Nasser, Mrs. Nicholas (Adele Achem)"	"female"	14	1	0	"237736"	30.0708	""	"C"
29	1	3	"O'Dwyer, Miss. Ellen \"Nellie\""	"female"	NA	0	0	"330959"	7.8792	""	"Q"

-}
module Numeric.Datasets.Titanic (titanicRemote, titanicLocal, TitanicEntry(..), Class(..), Age(..), Sex(..))where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Network.HTTP.Req ((/:), https, Scheme(..))

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

-- | The "Age" field requires a custom FromField instance because its value may be "NA"
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

-- | The Titanic dataset, to be downloaded from <https://raw.githubusercontent.com/JackStat/6003Data/master/Titanic.txt>
titanicRemote :: Dataset 'Https TitanicEntry
titanicRemote = withPreprocess removeEscQuotes $ csvHdrDatasetSep '\t' $ URL $ https "raw.githubusercontent.com" /: "JackStat" /: "6003Data" /: "master" /: "Titanic.txt"

-- | The Titanic dataset, parsed from a local copy
titanicLocal :: Dataset h TitanicEntry
titanicLocal = withPreprocess removeEscQuotes $ csvHdrDatasetSep '\t' $ File "datafiles/titanic2_full.tsv"





