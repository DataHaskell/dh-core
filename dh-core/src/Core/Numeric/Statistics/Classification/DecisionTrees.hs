{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Core.Numeric.Statistics.Classification.DecisionTrees where

import qualified Data.Foldable as F
import qualified Data.Set as S
import Data.Ord (comparing)

import Core.Data.Dataset
import qualified Core.Data.Datum.Vector as XV
-- import Core.Numeric.Statistics.Classification.Utils
import Core.Numeric.Statistics.InformationTheory (entropyR)

-- | A binary tree.
--
-- Each leaf carries data of type 'a' and we can attach metadata of type 'd' at each branching point.
data Tree d a =
    Node d (Tree d a) (Tree d a)
  | Leaf a
  deriving (Eq, Show, Functor, Foldable, Traversable)

unfoldTree :: (t -> Either a (d, t, t)) -> t -> Tree d a
unfoldTree f x =
  either Leaf (\(d, l, r) -> Node d (unfoldTree f l) (unfoldTree f r) ) (f x)


-- | Tree state : list of candidate dataset cuts (feature #, level)
data TState k a  = TState {
    tsFeatCuts :: S.Set (Int, a)
  , tsDataset :: Dataset k [XV.V a] } 

-- | Tree state + local tree depth
data TSd k a = TSd { tsDepth :: !Int, tState :: TState k a }

-- | Global options for growing decision trees
data TOptions = TOptions {
    toMaxDepth :: !Int  -- ^ Max tree depth
  , toMinLeafSize :: !Int  -- ^ Minimum size of the contents of a leaf
  , toOrder :: Order -- ^ Less than | Equal or larger than
  } deriving (Eq, Show)

-- | Tree node metadata
--
-- For decision trees, at each node we store the decision feature and its decision threshold
data TNData a = TNData {
    tJStar :: !Int  -- ^ Decision feature index
  , tTStar :: a  -- ^ Decision threshold
  } deriving (Eq)

instance Show a => Show (TNData a) where
  show (TNData j t) = unwords ["(j =", show j, ", t =", show t, ")"]


-- | Split decision: find feature (value, index) that maximizes the entropy drop (i.e the information gain, or KL divergence between the joint and factored datasets)
--
-- NB generates empty leaves
treeUnfoldStep :: (Ord a, Ord k) =>
              TOptions
           -> TSd k a
           -> Either (Dataset k [XV.V a]) (TNData a, TSd k a, TSd k a)
treeUnfoldStep (TOptions maxdepth minls ord) (TSd depth tst)
  | depth >= maxdepth || sizeDs tst <= minls = Left (tsDataset tst)
  | sizeDs tsl == 0 = Left (tsDataset tsr)
  | sizeDs tsr == 0 = Left (tsDataset tsl)
  | otherwise = Right (mdata, tdsl, tdsr)
  where
    sizeDs = size . tsDataset
    mdata = TNData jstar tstar
    (jstar, tstar, tsl, tsr) = maxInfoGainSplit ordf tst
    ordf = fromOrder ord
    d' = depth + 1
    tdsl = TSd d' tsl
    tdsr = TSd d' tsr



{- | Note (OPTIMIZATIONS maxInfoGainSplit)

1. After splitting a dataset, remove the (threshold, feature index) pair corresponding to the succesful split

2. " " " " , remove /all/ (threshold, index) pairs that are subsumed by the successful test (e.g in the test ((<), 3.2, 27) , remove all [(t, 27) | t <- [tmin ..], t < 3.2 ] ). This is only a useful optimization for /monotonic/ class boundaries.
-}

-- | Tabulate the information gain for a number of decision thresholds and return a decision function corresponding to the threshold that yields the maximum information gain.
maxInfoGainSplit :: (Ord k, Ord a, Eq a) =>
                    (a -> a -> Bool)
                 -> TState k a
                 -> (Int, a, TState k a, TState k a)
maxInfoGainSplit decision (TState tjs ds) = (jstar, tstar, TState tjs' dsl, TState tjs' dsr) where
  tjs' = S.delete (jstar, tstar) tjs  -- See Note (OPTIMIZATIONS maxInfoGainSPlit)
  (jstar, tstar, _, dsl, dsr) = F.maximumBy (comparing third5) $ infog `map` S.toList tjs
  infog (j, t) = (j, t, h, dsl', dsr') where
    (h, dsl', dsr') = infoGainR (decision t) j ds


third5 :: (a, b, c, d, e) -> c
third5 (_, _, c, _, _) = c

-- | Information gain due to a dataset split (regularized, H(0) := 0)
infoGainR :: (Ord k, Ord h, Floating h) =>
             (a -> Bool)
          -> Int
          -> Dataset k [XV.V a]
          -> (h, Dataset k [XV.V a], Dataset k [XV.V a])
infoGainR p j ds = (infoGain, dsl, dsr) where
    (dsl, pl, dsr, pr) = splitDatasetAtAttr p j ds
    (h0, hl, hr) = (entropyR ds, entropyR dsl, entropyR dsr)
    infoGain = h0 - (pl * hl + pr * hr)


-- | helper function for 'infoGain' and 'infoGainR'
splitDatasetAtAttr :: (Fractional n, Ord k) =>
                      (a -> Bool)
                   -> Int
                   -> Dataset k [XV.V a]
                   -> (Dataset k [XV.V a], n, Dataset k [XV.V a], n)  
splitDatasetAtAttr p j ds = (dsl, pl, dsr, pr) where
  sz = fromIntegral . size 
  (dsl, dsr) = partition p j ds
  (s0, sl, sr) = (sz ds, sz dsl, sz dsr)
  pl = sl / s0
  pr = sr / s0 


-- | Partition a Dataset in two, according to a decision predicate applied to a given feature.
--
-- e.g. "is the j'th component of datum X_i larger than threshold t ?" 
partition :: (Foldable t, Ord k) =>
              (a -> Bool) -- ^ Decision function (element-level)
           -> Int           -- ^ Feature index
           -> Dataset k (t (XV.V a))
           -> (Dataset k [XV.V a], Dataset k [XV.V a])
partition p j ds@Dataset{} = foldrWithKey insf (empty, empty) ds where
  insf k lrow (l, r) = (insert k lp l, insert k rp r) where    
    (lp, rp) = partition1 (XV.dataSplitDecision p j) lrow    



-- | Partition a Foldable in two lists according to a predicate
partition1 :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
partition1 p = foldr ins ([], [])  where
  ins x (l, r) | p x = (x : l, r)
               | otherwise = (l , x : r)



-- | A well-defined Ordering, for strict half-plane separation
data Order = LessThan | GreaterOrEqual deriving (Eq, Ord, Enum, Bounded)
instance Show Order where
  show LessThan = "<"
  show GreaterOrEqual = ">="

fromOrder :: Ord a => Order -> (a -> a -> Bool)
fromOrder o = case o of
  LessThan -> (<)
  _ -> (>=)
