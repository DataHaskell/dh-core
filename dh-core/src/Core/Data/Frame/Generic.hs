{-# language DataKinds, FlexibleContexts, GADTs #-}
{-# language LambdaCase #-}
{-# language DeriveDataTypeable, DeriveGeneric#-}
module Core.Data.Frame.Generic (
  gToTable,
  -- * DEBUG
  gToRow, gToRowMaybe
  ) where

import Generics.SOP (Generic(..), All, Code)
-- import Generics.SOP.NP
-- import qualified GHC.Generics as G
import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow(..))
import Data.Data (Data(..), constrFields)
import Data.Typeable (Typeable(..))

import qualified Data.Foldable as F (Foldable(..)) 
import qualified Data.Text as T
-- import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable(..))

import Core.Data.Frame (Row, Table, fromKVs, fromList, mkRow)
import qualified Core.Data.Frame.Value as AV
import qualified Core.Data.Frame.Value as AV
import qualified Core.Data.Frame.Value.Generic as AVG
-- import Core.Data.Frame.Generic (DataException(..))

-- $setup
-- >>> :set -XDeriveDataTypeable
-- >>> :set -XDeriveGeneric
-- >>> import Generics.SOP (Generic(..), All, Code)
-- >>> import Generics.SOP.NP
-- >>> import qualified GHC.Generics as G
-- >>> import Data.Data (Typeable, Data(..), constrFields)
-- >>> data P1 = P1 Int Char deriving (Eq, Show, Data, G.Generic)
-- >>> instance Generic P1
-- >>> data P2 = P2 { p2i :: Int, p2c :: Char } deriving (Eq, Show, Data, G.Generic)
-- >>> instance Generic P2
-- >>> data Q = Q { q1 :: Maybe Int, q2 :: Either Double Char } deriving (Eq, Show, Data, G.Generic)
-- >>> instance Generic Q

-- | Populate a 'Table' using the rows' 'Data', 'G.Generic' and 'Generic' instances and throws a 'DataException' if the input data is malformed.
--
-- For example, a list of records having two fields each will produce a table with two columns, having the record field names as column labels.
--
-- All record fields in the input data must be of types that are instances of the 'AV.ToValueM' class (as prescribed by the @All ToValueM xs@ constraint).
-- 
-- This function only accepts record types that have named constructor fields, such as P2 and Q in the following code :
--
-- @
-- data P1 = P1 Int Char deriving (Eq, Show, Data, G.Generic)
-- instance Generic P1
-- 
-- data P2 = P2 { p2i :: Int, p2c :: Char } deriving (Eq, Show, Data, G.Generic)
-- instance Generic P2
--
-- data Q = Q { q1 :: Maybe Int, q2 :: Either Double Char } deriving (Eq, Show, Data, G.Generic)
-- instance Generic Q
-- @
--
-- >>> gToTable [P1 42 'z']
-- *** Exception: Anonymous records not implemented yet
-- >>> gToTable [P2 42 'z']
-- Table {tableRows = [("p2i",VInt 42),("p2c",VChar 'z')] :| []}
--
-- Test using 'Maybe' and 'Either' record fields :
--
-- >>> gToTable [Q (Just 42) (Left 1.2), Q (Just 1) (Right 'a'), Q Nothing (Right 'b')]
-- Table {tableRows = [("q2",VDouble 1.2),("q1",VInt 42)] :| [[("q2",VChar 'a'),("q1",VInt 1)],[("q2",VChar 'b')]]}
--
-- As can be seen 'Nothing' values are not inserted in the rows, which can be used to encode missing data features.
gToTable :: (Code a ~ '[xs], All AV.ToValueM xs, Data a, Generic a, MonadThrow m) =>
             [a]
          -> m (Table (Row T.Text AV.Value))
gToTable ds
  | null ds = throwM NoDataE 
  | otherwise = do
      constrs <- check (head ds)
      pure $ fromList $ map (gToRowMaybe constrs) ds

check :: (Data a, MonadThrow m) => a -> m [T.Text]
check d | null constrs = throwM AnonRecordE
        | otherwise = pure constrs where
  constrs = T.pack `map` constrFields (toConstr d)      


-- | Populate a 'Row' using the rows' 'Data', 'G.Generic' and 'Generic' instances 
gToRow :: (Code a ~ '[xs], Data a, Generic a, All AV.ToValue xs) =>
          [T.Text]
       -> a
       -> Row T.Text AV.Value
gToRow constrs d = fromKVs $ zip constrs (AVG.npToValue d)

-- | Populate a 'Row' using the rows' 'Data', 'G.Generic' and 'Generic' instances
--
-- NB The ToValueM currently supports only "shallow" Maybe and Either record fields, i.e. their fields must be concrete types (as in, @Either Int String@ or @Maybe Char@).
gToRowMaybe :: (Code a ~ '[xs], Data a, Generic a, All AV.ToValueM xs) =>
               [T.Text]
            -> a
            -> Row T.Text AV.Value
gToRowMaybe constrs d = mkRow (insertsMaybe kvs) where
  kvs = zip constrs (AVG.npToValueM d)





insertsMaybe :: (Foldable t, Eq k, Hashable k) =>
                t (k, Maybe v)
             -> HM.HashMap k v
insertsMaybe xs = F.foldl insf HM.empty xs where
  insf acc (k, vmay) = case vmay of
    Just v  -> HM.insert k v acc
    Nothing -> acc



-- | Exceptions related to the input data
data DataException =
  AnonRecordE  -- ^ Anonymous records not implemented yet
  | NoDataE    -- ^ Dataset has 0 rows
  deriving (Eq, Typeable)
instance Show DataException where
  show = \case
    AnonRecordE -> "Anonymous records not implemented yet"
    NoDataE -> "The dataset has 0 rows"
instance Exception DataException
