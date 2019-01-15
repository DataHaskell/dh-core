{-# language OverloadedStrings #-}
module Analyze.Dplyr where

import Analyze.RFrame
import Analyze.Common

import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable(..))

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


  

-- * Relational operations

-- | INNER JOIN
innerJoin :: (Hashable v, Eq v, Key k) =>
             k -> k -> RFrame k v -> RFrame k v -> Maybe [V.Vector v]
innerJoin k1 k2 (RFrame ks1 hm1 rows1) (RFrame ks2 hm2 rows2) =
  hjProbe k1 k2 hm1 hm2 rows1 rows2


hjProbe :: (Hashable v, Eq v, Foldable t, Key k) =>
           k
        -> k
        -> HM.HashMap k Int
        -> HM.HashMap k Int
        -> t (V.Vector v)
        -> t (V.Vector v)
        -> Maybe [V.Vector v]  -- ^ What rows are these?
hjProbe k1 k2 hm1 hm2 rows1 rows2 = F.foldlM insf [] rows1 where
  insf acc row1 = do
    v1  <- look k1 hm1 row1
    built <- hjBuild k2 hm2 rows2    
    v2s <- HM.lookup v1 built
    pure (v2s `mappend` acc)


-- | "build" phase of the hash-join algorithm
hjBuild :: (Hashable a, Eq a, Foldable t, Key k) =>
           k
        -> HM.HashMap k Int
        -> t (V.Vector a)     -- ^ Smaller of the two tables
        -> Maybe (HM.HashMap a [V.Vector a])    
hjBuild k hm = F.foldlM insf HM.empty where
  insf hmAcc vec = do
    v <- look k hm vec
    let hm' = HM.insertWith mappend v [vec] hmAcc
    pure hm'
      
         
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
