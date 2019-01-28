{-# language DataKinds, FlexibleContexts, GADTs #-}
module Core.Data.Frame.Value.Generic where

import Generics.SOP
import Generics.SOP.NP
import Generics.SOP.NS
import qualified GHC.Generics as G
import Data.Data (Data(..), DataType, Constr, isAlgType, dataTypeConstrs, indexConstr, constrFields, constrIndex, constrType, maxConstrIndex, readConstr)

import qualified Core.Data.Frame.Value as AV

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XDeriveDataTypeable
-- >>> :set -XDeriveGeneric
-- >>> :set -XGADTs
-- >>> import Generics.SOP (Generic(..), All, Code)
-- >>> import Generics.SOP.NP
-- >>> import qualified GHC.Generics as G
-- >>> data P = P Int Char deriving (Eq, Show, G.Generic)
-- >>> instance Generic P
-- >>> data Q = Q (Maybe Int) (Either Double Char) deriving (Eq, Show, G.Generic)
-- >>> instance Generic Q

-- | Encodes a record as a list of 'AV.Value' s.
--
-- This function computes the generic representation of the record and populates a list with the record entry values, in the order in which they appear in the record implementation.
--
-- For example :
--
-- @
-- data P = P Int Char deriving (Eq, Show, G.Generic)
-- instance Generic P
-- @
-- 
-- >>> npToValue $ P 132 'x'
-- [VInt 132,VChar 'x']
npToValue :: (Generic a, All AV.ToValue xs, Code a ~ '[xs]) => a -> [AV.Value]
npToValue = collapse_NP . gMapToValue . gToNP

gMapToValue :: All AV.ToValue xs => NP I xs -> NP (K AV.Value) xs
gMapToValue = hcmap (Proxy :: Proxy AV.ToValue) (mapIK AV.toValue)

-- | Encodes a record as a list of 'Maybe' 'AV.Value' s.
--
-- Optional record fields can be used to encode the common case of data rows having missing (NA) features.
--
-- @
-- data Q = Q (Maybe Int) (Either Double Char) deriving (Eq, Show, G.Generic)
-- instance Generic Q
-- @
--
-- >>> npToValueM $ Q (Just 42) (Right 'z')
-- [Just (VInt 42),Just (VChar 'z')]
-- >>> npToValueM $ Q Nothing (Left 13.2)
-- [Nothing,Just (VDouble 13.2)]
npToValueM :: (Generic a, All AV.ToValueM xs, Code a ~ '[xs]) => a -> [Maybe AV.Value]
npToValueM = collapse_NP . gMapToValueM . gToNP

gMapToValueM :: All AV.ToValueM xs => NP I xs -> NP (K (Maybe AV.Value)) xs
gMapToValueM = hcmap (Proxy :: Proxy AV.ToValueM) (mapIK AV.toValueM)

gToNP :: (Generic a, Code a ~ '[x]) => a -> NP I x
gToNP d = unZ $ unSOP (from d)
