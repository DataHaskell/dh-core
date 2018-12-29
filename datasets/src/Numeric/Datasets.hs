{- |

The datasets package defines two different kinds of datasets:

* small data sets which are directly (or indirectly with @file-embed@)
  embedded in the package as pure values and do not require
  network or IO to download the data set.

* other data sets which need to be fetched over the network with
  'getDataset' and are cached in a local temporary directory

This module defines the 'getDataset' function for fetching datasets
and utilies for defining new data sets. It is only necessary to import
this module when using fetched data sets. Embedded data sets can be
imported directly.

-}

{-# LANGUAGE OverloadedStrings, GADTs, DataKinds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numeric.Datasets (getDataset, Dataset(..), Source(..),
                         -- * Parsing datasets 
                         readDataset, ReadAs(..), csvRecord,
                        -- * Defining datasets
                        csvDataset, csvHdrDataset, csvHdrDatasetSep, csvDatasetSkipHdr,
                        jsonDataset,
                        -- * Modifying datasets
                        withPreprocess, withTempDir,                        
                        -- * Preprocessing functions
                        --
                        -- | These functions are to be used as first argument of 'withPreprocess'. They act on the individual text fields of the raw dataset in order to sanitize the input data to the parsers.
                        dropLines, fixedWidthToCSV, removeEscQuotes, fixAmericanDecimals,
                        -- ** Helper functions
                         parseReadField, parseDashToCamelField, 
                         yearToUTCTime, 
                        -- * Dataset sources
                        umassMLDB, uciMLDB) where

import Data.Csv
import System.FilePath
import System.Directory
import Data.Hashable
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Aeson as JSON
import Control.Applicative
import Data.Time
-- import qualified Network.Wreq as Wreq
import Data.Default.Class (Default(..))
import Network.HTTP.Req (req, runReq, Url, (/:), http, https, Scheme(..), LbsResponse, lbsResponse, responseBody, GET(..), NoReqBody(..), HttpMethod(..))
-- import Lens.Micro ((^.))

import Data.Char (ord, toUpper)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.ByteString.Lazy.Search (replace)

-- * Using datasets

-- | Load a dataset, using the system temporary directory as a cache
getDataset :: Dataset h a -> IO [a]
getDataset ds = do
  dir <- tempDirForDataset ds
  bs <- fmap (fromMaybe id $ preProcess ds) $ getFileFromSource dir $ source ds
  return $ readDataset (readAs ds) bs

-- | Get a ByteString from the specified Source
getFileFromSource ::
     FilePath  -- ^ Cache directory
  -> Source h  
  -> IO BL.ByteString
getFileFromSource cacheDir (URL url) = do
  createDirectoryIfMissing True cacheDir
  let fnm = cacheDir </> "ds" <> show (hash $ show url)
  ex <- doesFileExist fnm
  if ex
     then BL.readFile fnm
     else do
       rsp <- runReq def $ req GET url NoReqBody lbsResponse mempty 
       let bs = responseBody rsp
       BL.writeFile fnm bs
       return bs
getFileFromSource _ (File fnm) = 
  BL.readFile fnm  

-- | Parse a ByteString into a list of Haskell values
readDataset ::
     ReadAs a      -- ^ How to parse the raw data string 
  -> BL.ByteString -- ^ The data string
  -> [a]
readDataset JSON bs =
  case JSON.decode bs of
    Just theData ->  theData
    Nothing -> error "failed to parse json"
readDataset (CSVRecord hhdr opts) bs =
  case decodeWith opts hhdr bs of
    Right theData -> V.toList theData
    Left err -> error err
readDataset (CSVNamedRecord opts) bs =
  case decodeByNameWith opts bs of
    Right (_,theData) -> V.toList theData
    Left err -> error err

tempDirForDataset :: Dataset h a -> IO FilePath
tempDirForDataset ds =
  case temporaryDirectory ds of
    Nothing -> getTemporaryDirectory
    Just tdir -> return tdir

-- | A 'Dataset' source can be either a URL (for remotely-hosted datasets) or the filepath of a local file.
data Source h = URL (Url h)
              | File FilePath

-- | A dataset is a record telling us how to load the data
data Dataset h a = Dataset
  { source :: Source h  -- ^ Dataset source
  , temporaryDirectory :: Maybe FilePath  -- ^ Temporary directory (optional)
  , preProcess :: Maybe (BL.ByteString -> BL.ByteString)  -- ^ Dataset preprocessing function (optional)
  , readAs :: ReadAs a
  }

-- | ReadAs is a datatype to describe data formats that hold data sets
data ReadAs a where
  JSON :: FromJSON a => ReadAs a
  CSVRecord :: FromRecord a => HasHeader -> DecodeOptions -> ReadAs a
  CSVNamedRecord :: FromNamedRecord a => DecodeOptions -> ReadAs a

-- | A CSV record with default decoding options (i.e. columns are separated by commas)
csvRecord :: FromRecord a => ReadAs a
csvRecord = CSVRecord NoHeader defaultDecodeOptions

-- * Defining datasets

-- | Define a dataset from a source for a CSV file
csvDataset :: FromRecord a =>  Source h -> Dataset h a
csvDataset src = Dataset src Nothing Nothing csvRecord 

-- | Define a dataset from a source for a CSV file, skipping the header line
csvDatasetSkipHdr :: FromRecord a => Source h -> Dataset h a
csvDatasetSkipHdr src = Dataset src Nothing Nothing $ CSVRecord HasHeader defaultDecodeOptions


-- |Define a dataset from a source for a CSV file with a known header
csvHdrDataset :: FromNamedRecord a => Source h -> Dataset h a
csvHdrDataset src = Dataset src Nothing Nothing $ CSVNamedRecord defaultDecodeOptions

-- |Define a dataset from a source for a CSV file with a known header and separator
csvHdrDatasetSep :: FromNamedRecord a => Char -> Source h -> Dataset h a
csvHdrDatasetSep sepc src
   = Dataset src Nothing Nothing
       $ CSVNamedRecord defaultDecodeOptions { decDelimiter = fromIntegral (ord sepc)}

-- | Define a dataset from a source for a JSON file 
jsonDataset :: FromJSON a => Source h -> Dataset h a
jsonDataset src = Dataset src Nothing Nothing JSON


-- * Modifying datasets

-- | Include a preprocessing stage to a Dataset: each field in the raw data will be preprocessed with the given function.
withPreprocess :: (BL8.ByteString -> BL8.ByteString) -> Dataset h a -> Dataset h a
withPreprocess preF ds = ds { preProcess = Just preF}

-- | Include a temporary directory for caching the dataset after this has been downloaded one first time.
withTempDir :: FilePath -> Dataset h a -> Dataset h a
withTempDir dir ds = ds { temporaryDirectory = Just dir }



-- * Helper functions for parsing datasets

-- | Turn dashes to CamelCase
dashToCamelCase :: String -> String
dashToCamelCase ('-':c:cs) = toUpper c : dashToCamelCase cs
dashToCamelCase (c:cs) = c : dashToCamelCase cs
dashToCamelCase [] = []

-- | Parse a field, first turning dashes to CamelCase
parseDashToCamelField :: Read a => Field -> Parser a
parseDashToCamelField s =
  case readMaybe (dashToCamelCase $ unpack s) of
    Just wc -> pure wc
    Nothing -> fail "unknown"

-- | Parse something, based on its read instance
parseReadField :: Read a => Field -> Parser a
parseReadField s =
  case readMaybe (unpack s) of
    Just wc -> pure wc
    Nothing -> fail "unknown"

-- | Drop lines from a bytestring
dropLines :: Int -> BL.ByteString -> BL.ByteString
dropLines 0 s = s
dropLines n s = dropLines (n-1) $ BL.tail $ BL8.dropWhile (/='\n') s

-- | Turn US-style decimals starting with a period (e.g. .2) into something @cassava@ can parse (e.g. 0.2)
fixAmericanDecimals :: BL.ByteString -> BL.ByteString
fixAmericanDecimals = replace ",." (",0."::BL.ByteString)

-- | Convert a Fixed-width format to a CSV
fixedWidthToCSV :: BL.ByteString -> BL.ByteString
fixedWidthToCSV = BL8.pack . fnl . BL8.unpack where
  f [] = []
  f (' ':cs) = ',':f (chomp cs)
  f ('\n':cs) = '\n':fnl cs
  f (c:cs) = c:f cs
  fnl cs = f (chomp cs) --newline
  chomp (' ':cs) = chomp cs
  chomp (c:cs) = c:cs
  chomp [] = []

-- | Filter out escaped double quotes from a field
removeEscQuotes :: BL8.ByteString -> BL8.ByteString
removeEscQuotes = BL8.filter (/= '\"')    

-- * Helper functions for data analysis

-- | convert a fractional year to UTCTime with second-level precision (due to not taking into account leap seconds)
yearToUTCTime :: Double -> UTCTime
yearToUTCTime yearDbl =
  let (yearn,yearFrac)  = properFraction yearDbl
      dayYearBegin = fromGregorian yearn 1 1
      (dayn, dayFrac) = properFraction $ yearFrac * (if isLeapYear yearn then 366 else 365)
      day = addDays dayn dayYearBegin
      dt = secondsToDiffTime $ round $ dayFrac * 86400
  in UTCTime day dt



-- * URLs

-- | The UMass machine learning database
--
-- <http://mlr.cs.umass.edu/ml/machine-learning-databases>
umassMLDB :: Url 'Http
umassMLDB = http "mlr.cs.umass.edu" /: "ml" /: "machine-learning-databases"

-- | The UCI machine learning database
--
-- | <https://archive.ics.uci.edu/ml/machine-learning-databases>
uciMLDB :: Url 'Https
uciMLDB = https "archive.ics.uci.edu" /: "ml" /: "machine-learning-databases"

