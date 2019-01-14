{-# language DataKinds, FlexibleContexts, GADTs #-}
module Analyze.Values.Generic (npToValue) where

import Generics.SOP
import Generics.SOP.NP

import qualified Analyze.Values as AV

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

-- | Encodes a record as a list of 'AV.Value's.
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
npToValue = collapse_NP . gMapToValue . gToList

gMapToValue :: All AV.ToValue xs => NP I xs -> NP (K AV.Value) xs
gMapToValue = hcmap (Proxy :: Proxy AV.ToValue) (mapIK AV.toValue)

gToList :: (Generic a, Code a ~ '[x]) => a -> NP I x
gToList d = unZ $ unSOP (from d)
