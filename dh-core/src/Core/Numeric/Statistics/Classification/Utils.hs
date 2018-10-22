{-# language TypeFamilies #-}
module Core.Numeric.Statistics.Classification.Utils where

-- import qualified Data.Foldable as F (maximumBy, foldl', toList)
import qualified Data.IntSet as SI
-- import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM

import System.Random.MWC
import System.Random.MWC.Distributions
import Control.Monad.Primitive

import Control.Monad (foldM, replicateM)
import Data.Maybe (maybeToList)

-- import Control.Monad.Catch (MonadThrow(..))
-- import Numeric.Classification.Exceptions

-- * Bootstrap

-- | Non-parametric bootstrap
bootstrapNP :: (Indexed f, PrimMonad m, Ix f ~ Int) =>
               Int  -- ^ Number of samples
            -> Int  -- ^ Number of bootstrap resamples
            -> Gen (PrimState m)
            -> f a  -- ^ Dataset
            -> m [[a]]
bootstrapNP nsamples nboot gen mm = replicateM nboot (resample nsamples mm gen)

-- * 

-- | Sample with replacement
resample :: (Indexed f, PrimMonad m, Ix f ~ Int) =>
            Int -> f b -> Gen (PrimState m) -> m [b]
resample nsamples im gen = lookups (resampleIxs nsamples gen) im

-- | Sample without replacement : return a list of at most M unique random samples from an indexed map of size N : O(N)
sample :: (Indexed f, PrimMonad m, Ix f ~ Int) =>
          Int -> f b -> Gen (PrimState m) -> m [b]
sample nsamples im gen = lookups (sampleIxs nsamples gen) im

lookups :: (Monad m, Monoid (t b), Traversable t, Indexed f) =>
           (Int -> m (t (Ix f)))  -- ^ Sampling function
        -> f b
        -> m (t b)
lookups f mm = do
  ixs <- f (length mm)
  pure $ mconcat . maybeToList $ traverse (`ix` mm) ixs

-- -- | Lookup a random subset and its complement
-- lookups2 :: (Monad m, Monoid (t b), Traversable t, Indexed f) =>
--             (Int -> m (t (Ix f), t (Ix f)))
--          -> f b
--          -> m (t b, t b)
-- lookups2 f mm = do
--   (ixl, ixr) <- f (length mm)
--   let lookupf = mconcat . maybeToList . traverse (`ix` mm)
--   pure (lookupf ixl, lookupf ixr)
  

  

resampleIxs :: PrimMonad m => Int -> Gen (PrimState m) -> Int -> m [Int]
resampleIxs nsamples gen n = replicateM nsamples (uniformR (0, n - 1) gen)

sampleIxs :: PrimMonad m => Int -> Gen (PrimState m) -> Int -> m [Int]
sampleIxs nsamples gen n = SI.toList <$> sampleUniques nsamples gen n

-- | Random split based on extracting 'm' unique entries from a set of size 'n > m'
randomSplit :: PrimMonad m =>
               Int   -- ^ Number of samples
            -> Int   -- ^ Size of universe set
            -> Gen (PrimState m)
            -> m (SI.IntSet, SI.IntSet)
randomSplit nsamples n gen = do
  srand <- sampleUniques nsamples gen n
  let s0 = SI.fromList [0 .. n - 1]
      sDiff = s0 `SI.difference` srand
  pure (srand, sDiff)

-- | Stochastic random split of a set of size 'n', based on a Bernoulli trial of parameter '0 <= p <= 1'; /on average/, m = p * n samples will be inserted in the left set, and n - m will be inserted in the right one.
randomSplitBernoulli :: PrimMonad m =>
                Double  -- ^ Parameter of Bernoulli trial
             -> Int
             -> Gen (PrimState m)
             -> m (SI.IntSet, SI.IntSet)
randomSplitBernoulli p n gen = foldM insf (SI.empty, SI.empty) [0.. n-1] where
  insf (sl, sr) i = do
    c <- bernoulli p gen 
    pure $ if c then
      (SI.insert i sl, sr)
      else
      (sl, SI.insert i sr)



-- sampleNoReplace iml nsamples gen
--   | nsamples > n = pure $ throwM $ DimMismatchE "sampleIM" n nsamples
--   | otherwise = do


-- | Sample without replacement : choose a set S of M unique random samples from a population of size N
sampleUniques :: PrimMonad m =>
                 Int   -- ^ # of unique numbers to sample (M)
              -> Gen (PrimState m)
              -> Int   -- ^ Population size (N)
              -> m SI.IntSet   
sampleUniques nsamples gen n = foldM sample1 SI.empty [p .. n - 1] where
  p = n - nsamples + 1
  sample1 s j = do
    t <- uniformR (0, j) gen
    let set' =
          if not (SI.member t s)
          then
            SI.insert t s
          else
            SI.insert j s
    return set'

-- stest n ntot = withSystemRandom . asGenIO $ \g -> do
--   let set = S.fromList [0..ntot - 1]
--   sampleUniques set n g




-- | Indexable containers
class Foldable f => Indexed f where
  type Ix f :: *
  ix :: Ix f -> f a -> Maybe a

instance Indexed [] where
  type Ix [] = Int
  ix = indexSafe

instance Indexed IM.IntMap where
  type Ix IM.IntMap = IM.Key
  ix = IM.lookup


indexSafe :: Int -> [a] -> Maybe a
indexSafe i ll | i < length ll = Just $ ll !! i
               | otherwise = Nothing
