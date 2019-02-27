{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Core.Data.Datum.Vector where

import qualified Data.IntMap as IM
-- import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import Control.Monad.Catch (MonadThrow(..))
import Core.Numeric.Statistics.Classification.Exceptions

newtype FeatureLabels = FeatureLabels (IM.IntMap String) deriving (Eq, Show)

featureLabels :: MonadThrow m => Int -> [String] -> m FeatureLabels
featureLabels n ls
  | length ls == n = pure $ FeatureLabels $ IM.fromList $ zip [0..] ls
  | otherwise = throwM $ DimMismatchE "featureLabels" n (length ls)

lookupFeatureLabelUnsafe :: IM.Key -> FeatureLabels -> String
lookupFeatureLabelUnsafe i (FeatureLabels fl) = fl IM.! i

  
-- | A data point i.e. a vector in R^n
newtype V a = V (V.Vector a) deriving (Eq, Show, Functor, Foldable, Traversable, Applicative, Monad)

fromListV :: [a] -> V a
fromListV = V . V.fromList
{-# inline fromListV #-}

toListV :: V a -> [a]
toListV (V vv) = V.toList vv

zipV :: V a -> V b -> V (a, b)
zipV (V v1) (V v2) = V $ V.zip v1 v2
{-# inline zipV #-}

unzipV :: V (a, b) -> (V a, V b)
unzipV (V vs) = (V v1, V v2) where
  (v1, v2) = V.unzip vs
{-# inline unzipV #-}  

mkV :: MonadThrow m => Int -> V.Vector a -> m (V a)
mkV n xs | dxs == n = pure $ V xs
         | otherwise = throwM $ DimMismatchE "mkV" n dxs where
             dxs = V.length xs

indexUnsafe :: V a -> Int -> a
(V vv) `indexUnsafe` j = vv V.! j
{-# inline indexUnsafe #-}

(!) :: MonadThrow m => V a -> Int -> m a
v ! j | j >= 0 && j < d = pure $ v `indexUnsafe` j
             | otherwise = throwM $ IndexOobE "(!)" j 0 d where
                 d = dim v

dim :: V a -> Int
dim (V vv) = V.length vv
{-# inline dim #-}

foldrWithKey :: (Int -> a -> b -> b) -> b -> V a -> b
foldrWithKey f z vv = foldr ins z $ zipV (fromListV [0..]) vv where
  ins (i, x) acc = f i x acc
{-# inline foldrWithKey #-}  


dataSplitDecision :: (a -> Bool) -> Int -> (V a -> Bool)
dataSplitDecision p j dat = p (dat `indexUnsafe` j)

-- allComponents :: V (a -> Bool) -> V a -> Bool
-- allComponents ps dat = all (== True) $ f <$> vps where
--   vps = zipV ps dat
--   f (p, vi) = p vi
-- -- allComponents ps dat = all (== True) $ ps <*> dat 




-- | Vectors with measurable entries

-- data Measurable a = BoundedBoth a a deriving (Eq, Show)

-- newtype Xf f a = Xf (V.Vector (f a))

-- type XMeas = Xf Measurable
