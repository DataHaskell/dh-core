{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Core frame types and functions
module Analyze.RFrame where

import           Analyze.Common
import           Analyze.Decoding    (Decoder (..), decoderKeys, runDecoder)
import qualified Control.Foldl       as F
import           Control.Monad       (join)
import           Control.Monad.Catch (MonadThrow (..))
import qualified Data.Aeson          as A
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

-- | In-memory row-oriented frame with columns named by `k` and values by `v`
data RFrame k v = RFrame
  { -- | Ordered vector of column names
    _rframeKeys   :: !(Vector k)
  , -- | Quick lookup from column name to column index
    _rframeLookup :: !(HashMap k Int)
  , -- | Vector of rows. Each element should be the length of number of columns.
    _rframeData   :: !(Vector (Vector v))
  } deriving (Eq, Show, Functor)

-- | A simpler 'RFrame' for updates
data RFrameUpdate k v = RFrameUpdate
  { -- | Ordered vector of column names
    _rframeUpdateKeys :: !(Vector k)
  , -- | Vector of rows.
    _rframeUpdateData :: !(Vector (Vector v))
  } deriving (Eq, Show, Functor)

-- | Alias for a function to be applied to each row
type RFrameMap k v a = Vector k -> HashMap k Int -> Int -> Vector v -> a

-- | Alias for a row filter
type RFrameFilter k v = RFrameMap k v Bool

-- | Prettier alias for getting the keys of an 'RFrame'
rframeKeys :: RFrame k v -> Vector k
rframeKeys = _rframeKeys

-- | Prettier alias for getting the data matrix of an 'RFrame'
rframeData :: RFrame k v -> Vector (Vector v)
rframeData = _rframeData

-- | An empty frame with no rows or columns
empty :: RFrame k v
empty = RFrame V.empty HM.empty V.empty

-- | Build an 'RFrame' from an 'RFrameUpdate'.
--   Throws on duplicate keys.
fromUpdate :: (Data k, MonadThrow m) => RFrameUpdate k v -> m (RFrame k v)
fromUpdate (RFrameUpdate ks vs) = checkForDupes ks >> pure (RFrame ks (makeLookup ks) vs)

-- | Build an 'RFrameUpdate' from an 'RFrame'
toUpdate :: Data k => RFrame k v -> RFrameUpdate k v
toUpdate (RFrame ks _ vs) = RFrameUpdate ks vs

-- | Number of columns in an 'RFrame'
numCols :: RFrame k v -> Int
numCols (RFrame ks _ _) = V.length ks

-- | Number of rows in an 'RFrame'
numRows :: RFrame k v -> Int
numRows (RFrame _ _ vs) = V.length vs

-- | Project to the given column
col :: (Data k, MonadThrow m) => k -> RFrame k v -> m (Vector v)
col k (RFrame _ look vs) = V.mapM (\v -> runLookup look v k) vs

-- | Decode by row. Each element of the returned vector may fail on decoding error
--   so flatten manually or use 'flatDecode'.
decode :: (Data k, MonadThrow m) => Decoder m k v a -> RFrame k v -> m (Vector (m a))
decode decoder rframe@(RFrame ks look vs) = checkSubset required keySet >> pure decoded
  where
    keySet = HS.fromList (V.toList ks)
    required = decoderKeys decoder
    decoded = runDecoder decoder . runLookup look <$> vs

-- | An auto-flattened version of 'decode'.
flatDecode :: (Data k, MonadThrow m) => Decoder m k v a -> RFrame k v -> m (Vector a)
flatDecode decoder rframe = join $ sequence <$> decode decoder rframe

-- | Filter an 'RFrame' by row
filter :: Data k => RFrameFilter k v -> RFrame k v -> RFrame k v
filter p (RFrame ks look vs) = RFrame ks look vs'
  where
    vs' = V.ifilter (p ks look) vs

-- | Update row-wise, adding or replacing values per-column.
--   Retains the existing column order, appending new columns.
--   Throws on row length mismatch or duplicate columns in the update.
update :: (Data k, MonadThrow m) => RFrameUpdate k v -> RFrame k v -> m (RFrame k v)
update (RFrameUpdate uks uvs) (RFrame fks look fvs) = do
  let fSize = V.length fvs
      uSize = V.length uvs
  if fSize /= uSize
    then throwM (RowSizeMismatch fSize uSize)
    else do
      checkForDupes uks
      let kis = mergeKeys fks uks
          ks' = (\(k, _, _) -> k) <$> kis
          look' = makeLookup ks'
          vs' = V.zipWith (runIndexedLookup kis) fvs uvs
      return (RFrame ks' look' vs')

-- | Split columns in an 'RFrame' by a predicate.
splitCols :: Data k => (k -> Bool) -> RFrame k v -> (RFrame k v, RFrame k v)
splitCols p (RFrame ks look vs) = (RFrame keepKs keepLook keepVs, RFrame dropKs dropLook dropVs)
  where
    (keepKs, dropKs) = V.partition p ks
    keepLook = makeLookup keepKs
    keepVs = reorder keepKs look <$> vs
    dropLook = makeLookup dropKs
    dropVs = reorder dropKs look <$> vs

-- | Drop columns in an 'RFrame' by a predicate.
dropCols :: Data k => (k -> Bool) -> RFrame k v -> RFrame k v
dropCols p frame = snd (splitCols p frame)

-- | Keep columns in an 'RFrame' by a predicate.
keepCols :: Data k => (k -> Bool) -> RFrame k v -> RFrame k v
keepCols p frame = fst (splitCols p frame)

-- | Appends rows to an 'RFrame', retaining column order of the first.
--   Throws on column mismatch.
appendRows :: (Data k, MonadThrow m) => RFrame k v -> RFrame k v -> m (RFrame k v)
appendRows (RFrame ks0 look0 vs0) (RFrame ks1 look1 vs1) = do
  checkReorder ks0 ks1
  let vs1' = reorder ks0 look1 vs1
  return (RFrame ks0 look0 (vs0 V.++ vs1'))

-- | Appends columns to an 'RFrame', retaining column order of the first.
extendCols :: (Data k, MonadThrow m) => RFrame k v -> RFrame k v -> m (RFrame k v)
extendCols f g = update (toUpdate g) f

-- | Takes first 'n' rows of an 'RFrame'.
takeRows :: Int -> RFrame k v -> RFrame k v
takeRows n (RFrame ks look vs) = RFrame ks look (V.take n vs)

-- | Adds a 'Vector' column to the 'RFrame'
addColumn :: (Data k, MonadThrow m) => RFrame k v -> k -> Vector v -> m (RFrame k v)                         
addColumn rf name v = do
  c <- newRFrameColumn name $ V.singleton <$> v
  extendCols rf c
 where
  newRFrameColumn rfName = fromUpdate . RFrameUpdate (V.singleton rfName)
