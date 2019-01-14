{-# language LambdaCase #-}
{-# language DeriveDataTypeable #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
module Analyze.RFrame.Generic (gToRFrame, DataException(..)) where

import Generics.SOP (Generic(..), All, Code)
import Generics.SOP.NP
import qualified GHC.Generics as G

import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow(..))
import Data.Data (Typeable, Data(..), constrFields)

import qualified Data.Foldable as F (Foldable(..)) 

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

import qualified Analyze.RFrame as AR
import qualified Analyze.Values as AV
import qualified Analyze.Values.Generic as AVG


-- | Populates an RFrame using the rows' 'Data', 'G.Generic' and 'Generic' instances and throws a 'DataException' if the input data is malformed. This function only accepts records that have named constructor fields, such as P2 in the following:
--
-- @
-- data P1 = P1 Int Char deriving (Eq, Show, Data, G.Generic)
-- instance Generic P1
-- 
-- data P2 = P2 { p2i :: Int, p2c :: Char } deriving (Eq, Show, Data, G.Generic)
-- instance Generic P2
-- @
--
-- >>> gToRFrame [P2 1 'a', P2 42 'z']
-- RFrame {_rframeKeys = ["p2i","p2c"], _rframeLookup = fromList [("p2i",0),("p2c",1)], _rframeData = [[VInt 1,VChar 'a'],[VInt 42,VChar 'z']]}
--
-- >>> gToRFrame [P1 1 'a', P1 42 'z']
-- *** Exception: Anonymous records not implemented yet
gToRFrame :: (Generic a, Data a, All AV.ToValue xs, Code a ~ '[xs]
            , MonadThrow m
            , Foldable t, Functor t) =>
              t a
           -> m (AR.RFrame T.Text AV.Value)
gToRFrame ds
  | null ds = throwM NoDataE
  | null constrs = throwM AnonRecordE
  | otherwise = pure $ AR.RFrame vc cm vss
  where
    dsl = F.toList ds
    d = head dsl 
    vss = V.fromList $ F.toList (V.fromList . AVG.npToValue <$> ds)
    cm = HM.fromList $ zip (T.pack `map` constrs) [0 ..]
    constrs = constrFields (toConstr d)
    vc = T.pack <$> V.fromList constrs

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
