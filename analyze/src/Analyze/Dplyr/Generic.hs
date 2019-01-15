{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
module Analyze.Dplyr.Generic (gToTable) where


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


gToTable :: (Code a ~ '[xs], All AV.ToValue xs, Data a, Generic a, MonadThrow m) =>
             [a]
          -> m (Table (Row T.Text AV.Value))
gToTable ds
  | null ds = throwM NoDataE
  | null constrs = throwM AnonRecordE
  | otherwise = pure $ fromList $ mkRow <$> ds
  where
    d = head ds
    mkRow x = fromListR $ zip constrs (AVG.npToValue x)
    constrs = T.pack `map` constrFields (toConstr d)


