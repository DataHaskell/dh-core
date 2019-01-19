{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- {-# language DeriveGeneric, DeriveDataTypeable #-}
module Analyze.Frame.Sparse (
  -- * Table
  Table,
  -- ** Construction
  fromNEList, fromList,
  -- ** Access
  head, zipWith, unionRowsWith,
  -- ** Filtering 
  filter, filterByKey,
  -- ** Scans (row-wise cumulative operations)
  scanl, scanr, 
  -- * Row
  Row,
  -- ** Construction 
  fromKVs,
  -- *** (unsafe)
  mkRow, 
  -- ** Access
  keys, elems, toList, 
  -- ** Traversal
  traverseWithKey, 
  -- ** Lookup 
  lookup, -- lookupDefault,
  -- ** Insertion 
  insert, insertRowFun, insertRowFunM, 
  -- ** Set-like row operations
  union, unionWith,
  -- ** Row functions
  elemSatisfies, (!:),  
  -- * Relational operations
  groupBy, innerJoin) where

import Data.Maybe (fromMaybe)
import Control.Applicative (Alternative(..))
import qualified Data.Foldable as F
-- import qualified Data.Vector as V
-- import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import Data.Hashable (Hashable(..))
import Control.Monad.Catch(MonadThrow(..))

import qualified Analyze.Decoding as D (Decode(..), mkDecode, (>>>))
import Analyze.Decoding ((>>>))
import Analyze.Common (Key, MissingKeyError(..))
import Analyze.Values


import Prelude hiding (filter, zipWith, lookup, scanl, scanr, head, getChar)

-- $setup
-- >>> let row0 = fromKVs [(0, 'a'), (3, 'b')] :: Row Int Char
-- >>> let row1 = fromKVs [(0, 'x'), (1, 'b'), (666, 'z')] :: Row Int Char 

t0 :: Table (Row String String)
t0 = fromList [ book1, ball, bike, book2 ] 
           where
             book1 = fromKVs [("item", "book"), ("id.0", "129"), ("qty", "1")]
             book2 = fromKVs [("item", "book"), ("id.0", "129"), ("qty", "5")]  
             ball = fromKVs [("item", "ball"), ("id.0", "234"), ("qty", "1")]  
             bike = fromKVs [("item", "bike"), ("id.0", "410"), ("qty", "1")]

t1 :: Table (Row String String)
t1 = fromList [ r1, r2, r3, r4 ] :: Table (Row String String)
           where
             r1 = fromKVs [("id.1", "129"), ("price", "100")]
             r2 = fromKVs [("id.1", "234"), ("price", "50")]  
             r3 = fromKVs [("id.1", "3"), ("price", "150")]
             r4 = fromKVs [("id.1", "99"), ("price", "30")]



-- | A 'Row' type is internally a hashmap:
--
-- * /O(log n)/ random access
-- * /O(n log n)/ set operations
-- * Supports missing elements
newtype Row k v = Row { unRow :: HM.HashMap k v } deriving (Eq, Functor, Foldable, Traversable)
instance (Show k, Show v) => Show (Row k v) where
  show = show . HM.toList . unRow

-- | Construct a 'Row' from a list of key-element pairs.
--
-- >>> lookup 3 (fromKVs [(3,'a'),(4,'b')])
-- Just 'a'
-- >>> lookup 6 (fromKVs [(3,'a'),(4,'b')])
-- Nothing
fromKVs :: (Eq k, Hashable k) => [(k, v)] -> Row k v
fromKVs = Row . HM.fromList

-- | Wrap a HashMap into a Row constructor.
--
-- NB : This function is for internal use only. Do not use this function in application code, since it may break invariants such as key uniqueness.
mkRow :: HM.HashMap k v -> Row k v
mkRow = Row

-- | Access the key-value pairs contained in the 'Row'
toList :: Row k v -> [(k, v)]
toList = HM.toList . unRow

-- | Lookup the value stored at a given key in a row
--
-- >>> lookup 0 row0
-- Just 'a'
-- >>> lookup 1 row0
-- Nothing
lookup :: (Eq k, Hashable k) => k -> Row k v -> Maybe v
lookup k = HM.lookup k . unRow

-- -- | Like 'lookup', but throws a 'MissingKeyError' if the lookup is unsuccessful
-- lookupThrowM :: (MonadThrow m, Key k) =>
--                 k -> Row k v -> m v
-- lookupThrowM k r = maybe (throwM $ MissingKeyError k) pure (lookup k r)

-- withLookupThrowM :: (MonadThrow m, Key k) =>
--                     (v -> m a)
--                  -> k
--                  -> Row k v -> m a
-- withLookupThrowM fwith k r = maybe (throwM $ MissingKeyError k) fwith (lookup k r)

-- -- | Lookup a key using a default value for non-existing keys
-- --
-- -- >>> lookupDefault 'x' 0 row0
-- -- 'a'
-- -- >>> lookupDefault 'x' 2 row0
-- -- 'x'
-- lookupDefault :: (Eq k, Hashable k) => v -> k -> Row k v -> v
-- lookupDefault v k = HM.lookupDefault v k . unRow

decodeRow :: (Eq k, Hashable k) => Row k o -> D.Decode Maybe k o
decodeRow r = D.mkDecode (`lookup` r)

decodeCol :: (Eq k, Hashable k) => k -> D.Decode Maybe (Row k o) o
decodeCol k = D.mkDecode (lookup k)

decInt = D.mkDecode getInt
decInteger = D.mkDecode getInteger
decDouble = D.mkDecode getDouble
decChar = D.mkDecode getChar
decText = D.mkDecode getText

-- | Decode any numerical value into a real number
decodeReal :: D.Decode Maybe Value Double
decodeReal =
  (fromIntegral <$> decInt)     <|>
  decDouble                     <|>
  (fromIntegral <$> decInteger)

real :: (Eq k, Hashable k) => k -> D.Decode Maybe (Row k Value) Double
real k = decodeCol k >>> decodeReal




-- | Insert a key-value pair into a row and return the updated one
-- 
-- >>> keys $ insert 2 'y' row0
-- [0,2,3]
insert :: (Eq k, Hashable k) => k -> v -> Row k v -> Row k v
insert k v = Row . HM.insert k v . unRow

-- | List the keys of a given row
--
-- >>> keys row0
-- [0,3]
keys :: Row k v -> [k]
keys = HM.keys . unRow

-- | List the elements of a given row
--
-- >>> elems row0
-- "ab"
elems :: Row k v -> [v]
elems = HM.elems . unRow

-- | Traverse a 'Row' using a function of both the key and the element.
traverseWithKey :: Applicative f => (k -> a -> f b) -> Row k a -> f (Row k b)
traverseWithKey f r = Row <$> HM.traverseWithKey f (unRow r)

-- | Set union of two rows
--
-- >>> keys $ union row0 row1
-- [0,1,3,666]
union :: (Eq k, Hashable k) => Row k v -> Row k v -> Row k v
union r1 r2 = Row $ HM.union (unRow r1) (unRow r2)

-- | Set union of two rows, using a combining function for equal keys
unionWith :: (Eq k, Hashable k) =>
             (v -> v -> v) -> Row k v -> Row k v -> Row k v
unionWith f r1 r2 = Row $ HM.unionWith f (unRow r1) (unRow r2)

-- | Looks up a key from a row and applies a predicate to its value (if this is found). If no value is found at that key the function returns False.
--
-- This function is meant to be used as first argument to 'filter'.
--
-- >>> elemSatisfies (== 'a') 0 row0
-- True
-- >>> elemSatisfies (== 'a') 42 row0
-- False
elemSatisfies :: (Eq k, Hashable k) => (a -> Bool) -> k -> Row k a -> Bool
elemSatisfies f k row = maybe False f (lookup k row)

-- | Inline synonim for 'elemSatisfies'
(!:) :: (Eq k, Hashable k) => k -> (a -> Bool) -> Row k a -> Bool
k !: f = elemSatisfies f k 

-- | Creates or updates a column with a function of the whole row
insertRowFun :: (Eq k, Hashable k) => (Row k v -> v) -> k -> Row k v -> Row k v
insertRowFun f knew row = insert knew (f row) row

-- | Monadic version of 'insertRowFun'
insertRowFunM :: (Eq k, Hashable k, Monad m) => (Row k v -> m v) -> k -> Row k v -> m (Row k v)
insertRowFunM fm knew row = do
  y <- fm row
  pure $ insert knew y row




-- [NOTE : table Alternative instance] 
-- 
-- https://github.com/Gabriel439/Haskell-Bears-Library/blob/master/src/Bears.hs
--
-- 'Table' has Applicative and Alternative instances
-- -- *  for Alternative, we need the possibility of an empty table (to implement `empty`). Currently this is impossible due to the 'NonEmpty' list implementation.

-- [NOTE : column universe and table pretty printing]
--
-- Currently this 'Table' implementation doesn't know anything of its row type, including the type of its keys and values.
-- To pretty-print our tables, we'd like instead to know the "universe of columns", i.e. all possible columns used in every row (or at least in the first N rows)



-- | A 'Table' is a non-empty list of rows.
newtype Table row = Table {
    -- nTableRows :: Maybe Int  -- ^ Nothing means unknown
    tableRows :: NE.NonEmpty row } deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Take the first row of a 'Table'
--
-- >>> head (fromList [row0, row1]) == row0
-- True
head :: Table row -> row
head = NE.head . tableRows

-- | Construct a table given a non-empty list of rows
--
-- >>> (head <$> fromNEList [row0, row1]) == Just row0
-- True
-- >>> (head <$> fromNEList []) == Nothing
-- True
fromNEList :: [row] -> Maybe (Table row)
fromNEList l = Table <$> NE.nonEmpty l

-- | Construct a table given a list of rows. Crashes if the input list is empty
fromList :: [row] -> Table row
fromList = Table . NE.fromList 

zipWith :: (a -> b -> row)
        -> Table a -> Table b -> Table row
zipWith f tt1 tt2 = Table $ NE.zipWith f (tableRows tt1) (tableRows tt2)

unionRowsWith :: (Eq k, Hashable k) =>
                 (v -> v -> v) -> Table (Row k v) -> Table (Row k v) -> Table (Row k v)
unionRowsWith f = zipWith (unionWith f)

-- | Filters a 'Table' according to a predicate. Returns Nothing only if the resulting table is empty (i.e. if no rows satisfy the predicate).
--
filter :: (row -> Bool) -> Table row -> Maybe (Table row)
filter ff = fromNEList . NE.filter ff . tableRows

-- | Filter a table according to predicate applied to an element pointed to by a given key.
--
-- >>> numRows <$> filterByKey "item" (/= "book") t0
-- Just 2
filterByKey :: (Eq k, Hashable k) =>
               k            -- ^ Key
            -> (v -> Bool)  -- ^ Predicate to be applied to the element
            -> Table (Row k v)
            -> Maybe (Table (Row k v))
filterByKey k ff = filter (k !: ff)


-- | Left-associative scan
scanl :: (b -> a -> b) -> b -> Table a -> Table b
scanl f z tt = Table $ NE.scanl f z (tableRows tt)

-- | Right-associative scan
scanr :: (a -> b -> b) -> b -> Table a -> Table b
scanr f z tt = Table $ NE.scanr f z (tableRows tt)

-- | /O(n)/ Count the number of rows in the table
--
-- >>> numRows t0
-- 4
numRows :: Table row -> Int 
numRows = length . tableRows
     


-- * Relational operations

-- | GROUP BY : given a key and a table that uses it, split the table in multiple tables, one per value taken by the key.
--
-- >>> numRows <$> (groupBy "id.0" t0 >>= HM.lookup "129")
-- Just 2
groupBy :: (Foldable t, Hashable k, Hashable v, Eq k, Eq v) =>
           k  -- ^ Key to group by
        -> t (Row k v) -- ^ A @Table (Row k v)@ can be used here
        -> Maybe (HM.HashMap v (Table (Row k v)))
groupBy k tab = do
  groups <- groupL k tab
  pure $ fromList <$> groups

groupL :: (Foldable t, Hashable k, Hashable v, Eq k, Eq v) =>
         k -> t (Row k v) -> Maybe (HM.HashMap v [Row k v])
groupL k tab = F.foldlM insf HM.empty tab where
  insf acc row = do
    v <- lookup k row
    pure $ HM.insertWith (++) v [row] acc


-- | INNER JOIN : given two tables and one key from each, compute the tables' inner join using the keys as relations.
--
-- >>> head t0
-- [("id.0","129"),("qty","1"),("item","book")]
--
-- >>> head t1
-- [("id.1","129"),("price","100")]
-- 
-- >>> head <$> innerJoin "id.0" "id.1" t0 t1
-- Just [("id.1","129"),("id.0","129"),("qty","5"),("item","book"),("price","100")]
innerJoin :: (Foldable t, Hashable v, Hashable k, Eq v, Eq k) =>
             k  -- ^ Key into the first table
          -> k  -- ^ Key into the second table
          -> t (Row k v)  -- ^ First table
          -> t (Row k v)  -- ^ Second table
          -> Maybe (Table (Row k v))
innerJoin k1 k2 table1 table2 = fromList <$> F.foldlM insf [] table1 where
  insf acc row1 = do
    v <- lookup k1 row1
    matchRows2 <- matchingRows k2 v table2 <|> Just [] 
    let rows' = map (union row1) matchRows2
    pure (rows' ++ acc)

-- | Return all rows that match a value at a given key
matchingRows :: (Foldable t, Hashable v, Hashable k, Eq v, Eq k) =>
                k
             -> v
             -> t (Row k v)
             -> Maybe [Row k v]
matchingRows k v rows = do
  rowMap <- hjBuild k rows
  HM.lookup v rowMap

-- | "build" phase of the hash-join algorithm
hjBuild :: (Foldable t, Hashable a, Hashable k, Eq a, Eq k) =>
            k -> t (Row k a) -> Maybe (HM.HashMap a [Row k a])
hjBuild k = F.foldlM insf HM.empty where
  insf hmAcc row = do
    v <- lookup k row
    let hm' = HM.insertWith mappend v [row] hmAcc
    pure hm'





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







-- | Polymorphic representation. A bit verbose

-- newtype GTable f k v = GTable { gTableRows :: f (HM.HashMap k v) }

-- instance (Show k, Show v) => Show (GTable U k v) where
--   show (GTable rows) = show rows
-- instance (Eq k, Eq v) => Eq (GTable U k v) where
--   GTable rows1 == GTable rows2 = rows1 == rows2
-- instance (Show k, Show v) => Show (GTable B k v) where
--   show (GTable rows) = show rows
-- instance (Eq k, Eq v) => Eq (GTable B k v) where
--   GTable rows1 == GTable rows2 = rows1 == rows2  

-- newtype B rows = B (NE.NonEmpty rows) deriving (Eq, Show)

-- newtype U rows = U [rows] deriving (Eq, Show)

-- newtype BTable k v = BTable { unBTable :: GTable B k v } -- deriving (Eq, Show)
-- newtype UTable k v = UTable { unUTable :: GTable U k v }
