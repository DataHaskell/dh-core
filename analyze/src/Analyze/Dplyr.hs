{-# language OverloadedStrings #-}
module Analyze.Dplyr where

import Analyze.RFrame
import Analyze.Common

import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

import Prelude hiding (filter)


      
-- | Filter the RFrame rows according to a predicate applied to a column value
filterByKey :: Key k =>
               (v -> Bool) -- ^ Predicate 
            -> k           -- ^ Column key
            -> RFrame k v  
            -> Maybe (RFrame k v)
filterByKey qv k (RFrame ks hm vs) = do
  vsf <- V.filterM ff vs
  pure $ RFrame ks hm vsf
  where 
    ff vrow = do
      i <- HM.lookup k hm
      v <- vrow V.!? i
      pure $ qv v


  

innerJoin q row1 fr2 = undefined
  where

-- | Right half of the loop for a hash-join    
looks :: (Foldable t, Key k, Eq v) =>
         v
      -> k
      -> HM.HashMap k Int
      -> t (V.Vector v)
      -> Maybe (HM.HashMap k v, Maybe v)
looks v1 k2 hm2 vecs2 = F.foldlM insf z vecs2 
  where
    z = (HM.empty, Nothing)
    insf (acc, _) vec2 = do
      v2 <- look k2 hm2 vec2
      if v1 == v2
        then
          pure (acc, Just v2)
        else do
          let acc' = HM.insert k2 v2 acc
          pure (acc', Nothing)
      
    
look :: Key k =>
        k
     -> HM.HashMap k Int
     -> V.Vector b
     -> Maybe b
look k hm vrow = do
  i <- HM.lookup k hm
  vrow V.!? i

    


-- -- Playground

-- data Selection i = Row i | Range i i | Rows [i] deriving (Eq, Show)

-- rangeUnion (Range ra rb) (Range sa sb) = Range (min ra sa) (max rb sb)
