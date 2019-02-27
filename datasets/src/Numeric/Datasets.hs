{- |

The datasets package defines two different kinds of datasets:

* small data sets which are directly (or indirectly with `file-embed`)
  embedded in the package as pure values and do not require
  network or IO to download the data set.

* other data sets which need to be fetched over the network with
  `getDataset` and are cached in a local temporary directory

This module defines the `getDataset` function for fetching datasets
and utilies for defining new data sets. It is only necessary to import
this module when using fetched data sets. Embedded data sets can be
imported directly.

-}

{-# LANGUAGE OverloadedStrings, GADTs #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numeric.Datasets where

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
import Data.Char (ord)
import qualified Network.Wreq as Wreq
import Lens.Micro ((^.))

import Data.Char (toUpper)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.ByteString.Lazy.Search (replace)

-- * Using datasets

-- |Load a dataset, using the system temporary directory as a cache
getDataset :: Dataset a -> IO [a]
getDataset ds = do
  dir <- tempDirForDataset ds
  bs <- fmap (fromMaybe id $ preProcess ds) $ getFileFromSource dir $ source ds
  return $ readDataset (readAs ds) bs

-- |Read a ByteString into a Haskell value
readDataset :: ReadAs a -> BL.ByteString -> [a]
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

tempDirForDataset :: Dataset a -> IO FilePath
tempDirForDataset ds =
  case temporaryDirectory ds of
    Nothing -> getTemporaryDirectory
    Just tdir -> return tdir

data Source = URL String
            | File FilePath

-- | A dataset is a record telling us how to load the data

data Dataset a = Dataset
  { source :: Source
  , temporaryDirectory :: Maybe FilePath
  , preProcess :: Maybe (BL.ByteString -> BL.ByteString)
  , readAs :: ReadAs a
  }

-- | ReadAs is a datatype to describe data formats that hold data sets

data ReadAs a where
  JSON :: FromJSON a => ReadAs a
  CSVRecord :: FromRecord a => HasHeader -> DecodeOptions -> ReadAs a
  CSVNamedRecord :: FromNamedRecord a => DecodeOptions -> ReadAs a

csvRecord :: FromRecord a => ReadAs a
csvRecord = CSVRecord NoHeader defaultDecodeOptions

-- * Defining datasets

-- |Define a dataset from a pre-processing function and a source for a CSV file
csvDatasetPreprocess :: FromRecord a => (BL.ByteString -> BL.ByteString) -> Source -> Dataset a
csvDatasetPreprocess preF src = (csvDataset src) { preProcess = Just preF }
--  parseCSV preF <$> getFileFromSource cacheDir src

-- |Define a dataset from a source for a CSV file
csvDataset :: FromRecord a =>  Source -> Dataset a
csvDataset src = Dataset src Nothing Nothing $ CSVRecord NoHeader defaultDecodeOptions

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

-- |Define a dataset from a source for a JSON file -- data file must be accessible with HTTP, not HTTPS
jsonDataset :: FromJSON a => Source -> Dataset a
jsonDataset src = Dataset src Nothing Nothing JSON

-- | Get a ByteString from the specified Source
getFileFromSource :: FilePath -> Source -> IO (BL.ByteString)
getFileFromSource cacheDir (URL url) = do
  createDirectoryIfMissing True cacheDir
  let fnm = cacheDir </> "ds" <> show (hash url)

  ex <- doesFileExist fnm
  if ex
     then BL.readFile fnm
     else do
       rsp <- Wreq.get url
       let bs = rsp ^. Wreq.responseBody
       BL.writeFile fnm bs
       return bs
getFileFromSource _ (File fnm) = do
  BL.readFile fnm

-- * Helper functions for parsing

-- |Turn dashes to CamlCase
dashToCamelCase :: String -> String
dashToCamelCase ('-':c:cs) = toUpper c : dashToCamelCase cs
dashToCamelCase (c:cs) = c : dashToCamelCase cs
dashToCamelCase [] = []

-- | Parse a field, first turning dashes to CamlCase
parseDashToCamelField :: Read a => Field -> Parser a
parseDashToCamelField s =
  case readMaybe (dashToCamelCase $ unpack s) of
    Just wc -> pure wc
    Nothing -> fail "unknown"

-- | parse somethign, based on its read instance
parseReadField :: Read a => Field -> Parser a
parseReadField s =
  case readMaybe (unpack s) of
    Just wc -> pure wc
    Nothing -> fail "unknown"

-- |Drop lines from a bytestring
dropLines :: Int -> BL.ByteString -> BL.ByteString
dropLines 0 s = s
dropLines n s = dropLines (n-1) $ BL.tail $ BL8.dropWhile (/='\n') s

-- | Turn US-style decimals  starting with a period (e.g. .2) into something Haskell can parse (e.g. 0.2)
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
