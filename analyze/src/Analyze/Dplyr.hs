module Analyze.Dplyr where

import Analyze.RFrame
import Analyze.Common

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
  



-- -- Playground

-- data Selection i = Row i | Range i i | Rows [i] deriving (Eq, Show)

-- rangeUnion (Range ra rb) (Range sa sb) = Range (min ra sa) (max rb sb)
