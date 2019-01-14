{-# LANGUAGE OverloadedStrings #-}

-- | Functions to work with included datasets.
module Analyze.Datasets where

import           Analyze.IO.CSV
import           Analyze.RFrame       (RFrame)
import           Control.Monad.Catch  (MonadThrow (..))
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Paths_analyze

-- | Load an included dataset.
datasetWithHeader :: Text -> Text -> IO (RFrame Text Text)
datasetWithHeader a b = do
  let path = "datasets/" ++ T.unpack a ++ "/" ++ T.unpack b ++ ".csv"
  newPath <- getDataFileName path
  bs <- LBS.readFile newPath
  decodeWithHeader bs

-- | Load the "train" partition of the "titanic" dataset.
titanicTrain :: IO (RFrame Text Text)
titanicTrain = datasetWithHeader "titanic" "train"

-- | Load the "test" partition of the "titanic" dataset.
titanicTest :: IO (RFrame Text Text)
titanicTest = datasetWithHeader "titanic" "test"
