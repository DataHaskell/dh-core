{- |

The @datasets@ package defines three different kinds of datasets:

* Tiny datasets (up to a few tens of rows) are embedded as part of the library source code, as lists of values.

* Small data sets are embedded indirectly (via @file-embed@)
  in the package as pure values and do not require IO
  to be downloaded (i.e. the data is loaded and parsed at compile time).

* Larger data sets which need to be fetched over the network
  and are cached in a local temporary directory for subsequent use.

This module defines the 'getDataset' function for fetching datasets
and utilities for defining new data sets and modifying their options.
It is only necessary to import this module when using fetched data sets.
Embedded data sets can be used directly.

Please refer to the dataset modules for examples.

-}

{-# LANGUAGE OverloadedStrings, GADTs, DataKinds #-}
{-# LANGUAGE LambdaCase #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numeric.Datasets (getDataset, Dataset(..), Source(..), getDatavec, defaultTempDir, getFileFromSource,
                         -- * Parsing datasets
                         readDataset, safeReadDataset, ReadAs(..), csvRecord,
                        -- * Defining datasets
                        csvDataset, csvHdrDataset, csvHdrDatasetSep, csvDatasetSkipHdr,
                        jsonDataset,
                        -- ** Dataset options
                        withPreprocess, withTempDir,
                        -- ** Preprocessing functions
                        --
                        -- | These functions are to be used as first argument of 'withPreprocess' in order to improve the quality of the parser inputs.
                        dropLines, fixedWidthToCSV, removeEscQuotes, fixAmericanDecimals,
                        -- ** Helper functions
                         parseReadField, parseDashToCamelField,
                         yearToUTCTime,
                        -- * Dataset source URLs
                        umassMLDB, uciMLDB) where

import Data.Csv
import System.FilePath
import System.Directory
import Data.Hashable
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import Data.Aeson as JSON
import Control.Applicative
import Data.Time
import Data.Default.Class (Default(..))
import Network.HTTP.Req (req, runReq, Url, (/:), http, https, Scheme(..), LbsResponse, lbsResponse, responseBody, GET(..), NoReqBody(..), HttpMethod(..))
-- import Lens.Micro ((^.))

import Control.Exception.Safe
import Data.Char (ord, toUpper)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.ByteString.Lazy.Search (replace)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Vector.Generic (Vector)
import qualified Data.Vector         as VB
import qualified Data.Vector.Generic as V
import qualified Data.Attoparsec.ByteString.Lazy as Atto

-- * Using datasets

-- | Load a dataset into memory
getDataset :: (MonadThrow io, MonadIO io) => Dataset a -> io [a]
getDataset ds = VB.toList <$> getDatavec ds

-- | Load a dataset into memory as a vector
getDatavec :: (MonadThrow io, MonadIO io, Vector v a) => Dataset a -> io (v a)
getDatavec ds = liftIO $ do
  folder <- tempDirForDataset ds
  file <- getFileFromSource folder (source ds)
  safeReadDataset (readAs ds) (fromMaybe id (preProcess ds) file)

-- | Get a ByteString from the specified Source
getFileFromSource
  :: FilePath  -- ^ Cache directory
  -> Source
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
readDataset
  :: ReadAs a      -- ^ How to parse the raw data string
  -> BL.ByteString -- ^ The data string
  -> [a]
readDataset ra bs =
  case safeReadDataset ra bs of
    Left e    -> error (show e)
    Right dat -> VB.toList dat

-- | Read a ByteString into a Haskell value
safeReadDataset :: forall v a m . (Vector v a, MonadThrow m) => ReadAs a -> BL.ByteString -> m (v a)
safeReadDataset ra bs = either throwString pure $
  case ra of
    JSON ->  V.fromList <$> JSON.eitherDecode' bs
    CSVRecord hhdr opts -> V.convert <$> decodeWith opts hhdr bs
    CSVNamedRecord opts -> V.convert . snd <$> decodeByNameWith opts bs
    Parsable psr -> V.fromList <$> Atto.eitherResult (Atto.parse (Atto.many' psr) bs)

-- | Get a temporary directory for a dataset.
tempDirForDataset :: Dataset a -> IO FilePath
tempDirForDataset = defaultTempDir . temporaryDirectory

-- | Reify an optional temporary directory
defaultTempDir :: Maybe FilePath -> IO FilePath
defaultTempDir = \case
  Nothing -> getTemporaryDirectory
  Just tdir -> return tdir


-- | A 'Dataset' source can be either a URL (for remotely-hosted datasets) or the filepath of a local file.
data Source
  = forall h . URL (Url h)
  | File FilePath

-- | A 'Dataset' contains metadata for loading, caching, preprocessing and parsing data.
data Dataset a = Dataset
  { source :: Source    -- ^ Dataset source
  , temporaryDirectory :: Maybe FilePath  -- ^ Temporary directory (optional)
  , preProcess :: Maybe (BL.ByteString -> BL.ByteString)  -- ^ Dataset preprocessing function (optional)
  , readAs :: ReadAs a
  }

-- | ReadAs is a datatype to describe data formats that hold data sets
data ReadAs a where
  JSON :: FromJSON a => ReadAs a
  CSVRecord :: FromRecord a => HasHeader -> DecodeOptions -> ReadAs a
  CSVNamedRecord :: FromNamedRecord a => DecodeOptions -> ReadAs a
  Parsable :: Atto.Parser a -> ReadAs a

-- | A CSV record with default decoding options (i.e. columns are separated by commas)
csvRecord :: FromRecord a => ReadAs a
csvRecord = CSVRecord NoHeader defaultDecodeOptions

-- * Defining datasets

-- | Define a dataset from a source for a CSV file
csvDataset :: FromRecord a =>  Source   -> Dataset a
csvDataset src = Dataset src Nothing Nothing csvRecord

-- | Define a dataset from a source for a CSV file, skipping the header line
csvDatasetSkipHdr :: FromRecord a => Source -> Dataset a
csvDatasetSkipHdr src = Dataset src Nothing Nothing $ CSVRecord HasHeader defaultDecodeOptions


-- |Define a dataset from a source for a CSV file with a known header
csvHdrDataset :: FromNamedRecord a => Source -> Dataset a
csvHdrDataset src = Dataset src Nothing Nothing $ CSVNamedRecord defaultDecodeOptions

-- |Define a dataset from a source for a CSV file with a known header and separator
csvHdrDatasetSep :: FromNamedRecord a => Char -> Source -> Dataset a
csvHdrDatasetSep sepc src
   = Dataset src Nothing Nothing
       $ CSVNamedRecord defaultDecodeOptions { decDelimiter = fromIntegral (ord sepc)}

-- | Define a dataset from a source for a JSON file
jsonDataset :: FromJSON a => Source -> Dataset a
jsonDataset src = Dataset src Nothing Nothing JSON


-- * Modifying datasets

-- | Include a preprocessing stage to a Dataset: each field in the raw data will be preprocessed with the given function.
withPreprocess :: (BL8.ByteString -> BL8.ByteString) -> Dataset a -> Dataset a
withPreprocess preF ds = ds { preProcess = Just preF}

-- | Include a temporary directory for caching the dataset after this has been downloaded one first time.
withTempDir :: FilePath -> Dataset a -> Dataset a
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

-- | Parse a CSV field, based on its read instance
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

-- | Convert a fractional year to UTCTime with second-level precision (due to not taking into account leap seconds)
yearToUTCTime :: Double -> UTCTime
yearToUTCTime yearDbl = UTCTime day dt
  where
    (yearn,yearFrac)  = properFraction yearDbl
    dayYearBegin = fromGregorian yearn 1 1
    (dayn, dayFrac) = properFraction $ yearFrac * (if isLeapYear yearn then 366 else 365)
    day = addDays dayn dayYearBegin
    dt = secondsToDiffTime $ round $ dayFrac * 86400

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

