{-# LANGUAGE ConstraintKinds #-}

-- | Common internal things (no other internal deps)
module Analyze.Common where

import           Control.Exception
import           Control.Monad       (forM_, unless)
import           Control.Monad.Catch (MonadThrow (..))
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS
import           Data.Typeable       (Typeable)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import qualified Data.Text as T

-- | Column keys need to have equality and hashability.
class (Eq k, Hashable k, Show k, Typeable k) => Key k where
instance Key Int
instance Key T.Text

-- | flip <$>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) x f = f <$> x
{-# INLINE (<&>) #-}
infixl 1 <&>

-- | Exception for when a column is missing from a frame.
data MissingKeyError k = MissingKeyError k deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (MissingKeyError k)

-- | Exception for when a column is duplicated in a frame.
data DuplicateKeyError k = DuplicateKeyError k deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (DuplicateKeyError k)

-- | Exception for when frame column sizes don't match.
data ColSizeMismatch = ColSizeMismatch Int Int deriving (Show, Eq, Typeable)
instance Exception ColSizeMismatch

-- | Exception for when frame row sizes don't match.
data RowSizeMismatch = RowSizeMismatch Int Int deriving (Show, Eq, Typeable)
instance Exception RowSizeMismatch

-- | Throws when duplicate keys are present in a vector.
checkForDupes :: (Key k, MonadThrow m) => Vector k -> m ()
checkForDupes vs = go HS.empty (V.toList vs)
  where
    go _ [] = pure ()
    go s (k:ks) =
      if HS.member k s
        then throwM (DuplicateKeyError k)
        else go (HS.insert k s) ks

-- | Throws when one vector is not a reordering of the other.
checkReorder :: (Key k, MonadThrow m) => Vector k -> Vector k -> m ()
checkReorder xs ys =
  let xSize = V.length xs
      ySize = V.length ys
  in if xSize /= ySize
    then throwM (ColSizeMismatch xSize ySize)
    else checkSubset (V.toList xs) (HS.fromList (V.toList ys))

-- | Throws when any key is not present in the set.
checkSubset :: (Key k, MonadThrow m) => [k] -> HashSet k -> m ()
checkSubset qs ks = forM_ qs (\q -> unless (HS.member q ks) (throwM (MissingKeyError q)))

-- | Builds a reverse lookup for the vector.
makeLookup :: Key k => Vector k -> HashMap k Int
makeLookup = HM.fromList . flip zip [0..] . V.toList

-- | Indexes into the vector of values, throwing on key missing or bad index.
runLookup :: (Key k, MonadThrow m) => HashMap k Int -> Vector v -> k -> m v
runLookup look vs k =
  case HM.lookup k look >>= (vs V.!?) of
    Nothing -> throwM (MissingKeyError k)
    Just v  -> pure v

-- | Reorders the vector of values by a new key order and an old lookup.
reorder :: Key k => Vector k -> HashMap k Int -> Vector v -> Vector v
reorder ks look vs = pick <$> ks
  where
    pick k = vs V.! (look HM.! k)

-- | Merges two key vectors and tags each with its provenance (favoring the second).
mergeKeys :: Key k => Vector k -> Vector k -> Vector (k, Int, Int)
mergeKeys xs ys =
  let m = HM.fromList (V.toList (V.imap (\i x -> (x, (0, i))) xs))
      n = HM.fromList (V.toList (V.imap (\i x -> (x, (1, i))) ys))
      -- Ties go to the first argument, in this case favoring the update
      o = HM.union n m
      p = (\x -> let (a, b) = o HM.! x in (x, a, b)) <$> xs
      q = (\x -> let (a, b) = n HM.! x in (x, a, b)) <$> V.filter (\x -> not (HM.member x m)) ys
  in p V.++ q

-- | Uses a merged key vector to select values.
runIndexedLookup :: Vector (k, Int, Int) -> Vector v -> Vector v -> Vector v
runIndexedLookup ks xs ys = (\(_, i, j) -> (if i == 0 then xs else ys) V.! j) <$> ks
