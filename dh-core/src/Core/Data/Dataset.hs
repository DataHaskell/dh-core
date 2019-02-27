{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TypeFamilies #-}
module Core.Data.Dataset where

import qualified Data.Foldable as F (maximumBy)
import Data.Ord (comparing)

import qualified Data.Map.Strict as M (Map, empty, fromList, toList, fromListWith, mapWithKey, foldl', foldrWithKey, foldlWithKey', insert)
import qualified Data.Map.Internal.Debug as M (showTree)
-- import qualified Data.IntMap.Strict as IM
-- import qualified Data.Set as S

import System.Random.MWC
import Control.Monad.Primitive

import Core.Numeric.Statistics.Classification.Utils (Indexed(..), bootstrapNP)

-- | Labeled dataset represented as a 'Map'. The map keys are the class labels
newtype Dataset k a = Dataset { unDataset :: M.Map k a } deriving (Eq, Show, Functor, Foldable, Traversable)

showTree :: (Show k, Show a) => Dataset k a -> String
showTree (Dataset mm) = M.showTree mm

empty :: Dataset k a
empty = Dataset M.empty

insert :: Ord k => k -> a -> Dataset k a -> Dataset k a
insert k ls (Dataset ds) = Dataset $ M.insert k ls ds

mapWithKey :: (k -> a -> b) -> Dataset k a -> Dataset k b
mapWithKey f (Dataset ds) = Dataset $ M.mapWithKey f ds

foldrWithKey :: (k -> a -> b -> b) -> b -> Dataset k a -> b
foldrWithKey f z (Dataset ds) = M.foldrWithKey f z ds

foldlWithKey' :: (a -> k -> b -> a) -> a -> Dataset k b -> a
foldlWithKey' f z (Dataset ds) = M.foldlWithKey' f z ds

fromList :: Ord k => [(k, a)] -> Dataset k a
fromList ld = Dataset $ M.fromList ld

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Dataset k a
fromListWith f ld = Dataset $ M.fromListWith f ld

toList :: Dataset k a -> [(k, a)]
toList (Dataset ds) = M.toList ds

-- lookup :: Ord k => k -> Dataset k a -> Maybe a
-- lookup k (Dataset ds) = M.lookup k ds

-- | Size of the dataset
size :: Foldable t => Dataset k (t a) -> Int
size (Dataset ds) = M.foldl' (\acc l -> acc + length l) 0 ds

-- | Maximum likelihood estimate of class label
mlClass :: Dataset k [a] -> k
mlClass = fst . F.maximumBy (comparing f) . toList where
  f (_, ll) = length ll


-- | Number of items in each class
sizeClasses :: (Foldable t, Num n) => Dataset k (t a) -> M.Map k n
sizeClasses (Dataset ds) = (fromIntegral . length) <$> ds

-- | Empirical class probabilities i.e. for each k, number of items in class k / total number of items
probClasses :: (Fractional prob, Foldable t) => Dataset k (t a) -> M.Map k prob
probClasses ds = (\n -> n / fromIntegral (size ds)) <$> sizeClasses ds


-- * Bootstrap 

-- | Nonparametric bootstrap: each class is resampled (i.e. sampled with replacement)
bootstrap :: (Indexed f, PrimMonad m, Ix f ~ Int) =>
             Dataset k (f a)
          -> Int  -- ^ Number of samples
          -> Int  -- ^ Number of bootstrap resamples
          -> Gen (PrimState m)
          -> m [Dataset k [a]]
bootstrap ds@Dataset{} nsamples nboot gen = do 
  dss <- traverse (bootstrapNP nsamples nboot gen) ds
  pure $ sequenceA dss
