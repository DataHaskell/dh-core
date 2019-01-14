{-# language DeriveDataTypeable, DeriveGeneric #-}
{-# language GADTs, DataKinds, FlexibleContexts #-}
{-# language LambdaCase #-}
module Main where

import qualified Analyze.RFrame as AR
import qualified Analyze.Values as AV
-- import qualified Analyze.Values.Generic as AVG
import qualified Analyze.RFrame.Generic as ARG

import Analyze.Conversions (projectRow, projectRows)

import qualified GHC.Generics as G

-- | generics-sop
import Generics.SOP (Generic, All, Code)
-- -- | sop-core
-- import Generics.SOP.NP (collapse_NP)

-- | typeable
import Data.Data (Typeable, Data(..))

-- import Data.Hashable (Hashable(..))
-- import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow(..))

import qualified Data.Text as T
-- import qualified Data.Vector as V
-- import qualified Data.HashMap.Lazy as HM

import Numeric.Datasets (getDataset)
import Numeric.Datasets.Titanic

instance Generic TitanicEntry






  



main = putStrLn "hello!"






