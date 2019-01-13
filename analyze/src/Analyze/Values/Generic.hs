{-# language DataKinds, FlexibleContexts, GADTs #-}
module Analyze.Values.Generic (npToValue) where

import Generics.SOP
import Generics.SOP.NP

import qualified Analyze.Values as AV

-- | Python in Haskell yay!
npToValue :: (Generic a, All AV.ToValue xs, Code a ~ '[xs]) => a -> [AV.Value]
npToValue = collapse_NP . gMapToValue . gToList

gMapToValue :: All AV.ToValue xs => NP I xs -> NP (K AV.Value) xs
gMapToValue = hcmap (Proxy :: Proxy AV.ToValue) (mapIK AV.toValue)

gToList :: (Generic a, Code a ~ '[x]) => a -> NP I x
gToList d = unZ $ unSOP (from d)
