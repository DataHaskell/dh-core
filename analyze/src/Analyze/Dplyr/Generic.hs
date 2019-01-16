{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
module Analyze.Dplyr.Generic (gToTable, gToRow) where


import Generics.SOP (Generic(..), All, Code)
-- import Generics.SOP.NP
-- import qualified GHC.Generics as G

import Control.Monad.Catch (MonadThrow(..))
import Data.Data (Data(..), constrFields)

-- import qualified Data.Foldable as F (Foldable(..)) 

import qualified Data.Text as T
-- import qualified Data.Vector as V
-- import qualified Data.HashMap.Strict as HM

import Analyze.Dplyr (Row, Table, fromListR, fromList)
import qualified Analyze.Values as AV
import qualified Analyze.Values.Generic as AVG
import Analyze.RFrame.Generic (DataException(..))

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XDeriveDataTypeable
-- >>> :set -XDeriveGeneric
-- >>> :set -XGADTs
-- >>> import Generics.SOP (Generic(..), All, Code)
-- >>> import Generics.SOP.NP
-- >>> import qualified GHC.Generics as G
-- >>> import Data.Data (Typeable, Data(..), constrFields)
-- >>> data P1 = P1 Int Char deriving (Eq, Show, Data, G.Generic)
-- >>> instance Generic P1
-- >>> data P2 = P2 { p2i :: Int, p2c :: Char } deriving (Eq, Show, Data, G.Generic)
-- >>> instance Generic P2

-- | Populate a 'Table' using the rows' 'Data', 'G.Generic' and 'Generic' instances and throws a 'DataException' if the input data is malformed.
--
-- For example, a list of records having two fields each will produce a table with two columns, having the record field names as column labels.
--
-- All record fields in the input data must be of types that are instances of the 'AV.ToValue' class (as prescribed by the @All ToValue xs@ constraint).
-- 
-- This function only accepts records that have named constructor fields, such as P2 in the following code :
--
-- @
-- data P1 = P1 Int Char deriving (Eq, Show, Data, G.Generic)
-- instance Generic P1
-- 
-- data P2 = P2 { p2i :: Int, p2c :: Char } deriving (Eq, Show, Data, G.Generic)
-- instance Generic P2
-- @
--
-- >>> gToTable [P1 42 'z']
-- *** Exception: Anonymous records not implemented yet
-- >>> gToTable [P2 42 'z']
-- Table {tableRows = [("p2i",VInt 42),("p2c",VChar 'z')] :| []}
gToTable :: (Code a ~ '[xs], All AV.ToValue xs, Data a, Generic a, MonadThrow m) =>
             [a]
          -> m (Table (Row T.Text AV.Value))
gToTable ds
  | null ds = throwM NoDataE
  | otherwise = fromList <$> traverse gToRow ds 

-- | Populate a 'Row' using the rows' 'Data', 'G.Generic' and 'Generic' instances and throws a 'DataException' if the input data is malformed.
gToRow :: (MonadThrow m, Code a ~ '[xs], Data a, Generic a, All AV.ToValue xs) =>
          a
       -> m (Row T.Text AV.Value)
gToRow d | null constrs = throwM AnonRecordE
         | otherwise = pure $ fromListR $ zip constrs (AVG.npToValue d)
  where
    constrs = T.pack `map` constrFields (toConstr d) 


