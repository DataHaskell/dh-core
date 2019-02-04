{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Core frame types and functions
module Analyze.RFrame where

import           Analyze.Common
import           Analyze.Decoding    (Decoder (..), decoderKeys, runDecoder)
import qualified Data.Foldable       as F
-- import qualified Control.Foldl       as Foldl
import           Control.Monad       (join)
import           Control.Monad.Catch (MonadThrow (..))
-- import qualified Data.Aeson          as A
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
-- import           Data.Text           (Text)
-- import qualified Data.Text           as T
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
  } deriving (Eq, Show, Functor, Foldable, Traversable)

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

-- | Keys of an 'RFrame'
rframeKeys :: RFrame k v -> [k]
rframeKeys rf = HM.keys (_rframeLookup rf)

-- | Data matrix of an 'RFrame'
rframeData :: RFrame k v -> Vector (Vector v)
rframeData = _rframeData

-- | An empty frame with no rows or columns
empty :: RFrame k v
empty = RFrame V.empty HM.empty V.empty

-- | Build an 'RFrame' from an 'RFrameUpdate'.
--   Throws on duplicate keys.
fromUpdate :: (Key k, MonadThrow m) => RFrameUpdate k v -> m (RFrame k v)
fromUpdate (RFrameUpdate ks vs) = checkForDupes ks >> pure (RFrame ks (makeLookup ks) vs)

-- | Build an 'RFrameUpdate' from an 'RFrame'
toUpdate :: Key k => RFrame k v -> RFrameUpdate k v
toUpdate (RFrame ks _ vs) = RFrameUpdate ks vs

-- | Number of columns in an 'RFrame'
numCols :: RFrame k v -> Int
numCols = length . rframeKeys

-- | Number of rows in an 'RFrame'
numRows :: RFrame k v -> Int
numRows = length . rframeData

-- | Project to the given column
col :: (Key k, MonadThrow m) => k -> RFrame k v -> m (Vector v)
col k (RFrame _ look vs) = V.mapM (\v -> runLookup look v k) vs

-- | Decode by row. Each element of the returned vector may fail on decoding error
--   so flatten manually or use 'flatDecode'.
decode :: (Key k, MonadThrow m) => Decoder m k v a -> RFrame k v -> m (Vector (m a))
decode decoder (RFrame ks look vs) = checkSubset required keySet >> pure decoded
  where
    keySet = HS.fromList (V.toList ks)
    required = decoderKeys decoder
    decoded = runDecoder decoder . runLookup look <$> vs

-- | An auto-flattened version of 'decode'.
flatDecode :: (Key k, MonadThrow m) => Decoder m k v a -> RFrame k v -> m (Vector a)
flatDecode decoder rframe = join $ sequence <$> decode decoder rframe

-- -- | Filter an 'RFrame' by row
-- -- filter :: Key k => RFrameFilter k v -> RFrame k v -> RFrame k v
-- filter p (RFrame ks look vs) = RFrame ks look vs'
--   where
--     vs' = V.ifilter (p ks look) vs


-- -- | Filter the RFrame rows according to a predicate applied to a column value
-- filterByKey :: Key k =>
--                (v -> Bool) -- ^ Predicate 
--             -> k           -- ^ Column key
--             -> RFrame k v  
--             -> Maybe (RFrame k v)
-- filterByKey qv k (RFrame ks hm vs) = do
--   vsf <- V.filterM ff vs
--   pure $ RFrame ks hm vsf
--   where 
--     ff vrow = do
--       i <- HM.lookup k hm
--       v <- vrow V.!? i
--       pure $ qv v


-- | Update row-wise, adding or replacing values per-column.
--   Retains the existing column order, appending new columns.
--   Throws on row length mismatch or duplicate columns in the update.
update :: (Key k, MonadThrow m) => RFrameUpdate k v -> RFrame k v -> m (RFrame k v)
update (RFrameUpdate uks uvs) (RFrame fks _ fvs) = do
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
splitCols :: Key k => (k -> Bool) -> RFrame k v -> (RFrame k v, RFrame k v)
splitCols p (RFrame ks look vs) = (RFrame keepKs keepLook keepVs, RFrame dropKs dropLook dropVs)
  where
    (keepKs, dropKs) = V.partition p ks
    keepLook = makeLookup keepKs
    keepVs = reorder keepKs look <$> vs
    dropLook = makeLookup dropKs
    dropVs = reorder dropKs look <$> vs

-- | Drop columns in an 'RFrame' by a predicate.
dropCols :: Key k => (k -> Bool) -> RFrame k v -> RFrame k v
dropCols p frame = snd (splitCols p frame)

-- | Keep columns in an 'RFrame' by a predicate.
keepCols :: Key k => (k -> Bool) -> RFrame k v -> RFrame k v
keepCols p frame = fst (splitCols p frame)

-- | Appends rows to an 'RFrame', retaining column order of the first.
--   Throws on column mismatch.
appendRows :: (Key k, MonadThrow m) => RFrame k v -> RFrame k v -> m (RFrame k v)
appendRows (RFrame ks0 look0 vs0) (RFrame ks1 look1 vs1) = do
  checkReorder ks0 ks1
  let vs1' = reorder ks0 look1 vs1
  return (RFrame ks0 look0 (vs0 V.++ vs1'))

-- | Appends columns to an 'RFrame', retaining column order of the first.
extendCols :: (Key k, MonadThrow m) => RFrame k v -> RFrame k v -> m (RFrame k v)
extendCols f g = update (toUpdate g) f

-- | Takes first 'n' rows of an 'RFrame'.
takeRows :: Int -> RFrame k v -> RFrame k v
takeRows n (RFrame ks look vs) = RFrame ks look (V.take n vs)

-- | Adds a 'Vector' column to the 'RFrame'
addColumn :: (Key k, MonadThrow m) => RFrame k v -> k -> Vector v -> m (RFrame k v)
addColumn rf name v = do
  c <- newRFrameColumn name $ V.singleton <$> v
  extendCols rf c
 where
  newRFrameColumn rfName = fromUpdate . RFrameUpdate (V.singleton rfName)

-- | Projects values out of the map according to the given key order.
projectRow :: (Key k, MonadThrow m) => Vector k -> HashMap k v -> m (Vector v)
projectRow ks row = V.mapM f ks
  where
    f k =
      case HM.lookup k row of
        Nothing -> throwM (MissingKeyError k)
        Just v  -> pure v

-- | Projects an 'RFrame' out of many maps according to the given key order.
projectRows :: (Key k, MonadThrow m) => Vector k -> Vector (HashMap k v) -> m (RFrame k v)
projectRows ks rs = do
  vs <- V.mapM (projectRow ks) rs
  fromUpdate (RFrameUpdate ks vs)
