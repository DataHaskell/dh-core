{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Applicative decoding with key-value lookups.
--   Think of a 'Decoder' as a row function that exposes the columns it uses.
module Analyze.Decoding
  -- (
  --   -- * Decoder
  --   Decoder
  --   -- ** Construction
  -- , require
  -- , requireWhere
  -- -- ** Execution
  -- , runDecoder
  -- -- ** Utilities
  -- , decoderKeys
  -- )
  where

import           Analyze.Common           (Key)

import           Control.Applicative      (Alternative(..))
import Data.Foldable (Foldable(..), asum)
import           Control.Applicative.Free (Ap(..), liftAp)
-- import           Data.Maybe               (fromMaybe)

-- import Prelude hiding (lookup)


-- | We can decouple lookup and value conversion and have distinct error behaviour.
-- Multiple value decoding functions can be combined via the Alternative instance. 

newtype Decode i m o = Decode { runDecode :: i -> m o } deriving (Functor)

mkDecode :: (i -> m o) -> Decode i m o
mkDecode = Decode

instance Applicative m => Applicative (Decode i m) where
  pure x = Decode $ \ _ -> pure x
  Decode af <*> Decode aa = Decode $ \ v -> af v <*> aa v

instance Alternative m => Alternative (Decode i m) where
  empty = Decode $ const empty
  Decode p <|> Decode q = Decode $ \v -> p v <|> q v

-- Decode i m a -> (a -> Decode i m b) -> Decode i m b
-- (i -> m a)   -> (a -> (i -> m b)  ) -> (i -> m b)

-- spork :: Monad m => Decode i m a -> (a -> Decode i m b) -> Decode i m b
-- spork m k = Decode $ \ i -> do
--   a <- runDecode m i
--   _ a

-- instance Monad m => Monad (Decode i m) where




-- | 'Decode' is called Kleisli in base.Control.Arrow
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b } deriving Functor

instance Applicative m => Applicative (Kleisli m a) where
  pure x = Kleisli $ \ _ -> pure x
  Kleisli af <*> Kleisli aa = Kleisli $ \ v -> af v <*> aa v
 
instance Alternative m => Alternative (Kleisli m a) where
  empty = Kleisli $ const empty
  Kleisli p <|> Kleisli q = Kleisli $ \v -> p v <|> q v

instance Monad m => Profunctor (Kleisli m) where
  dimap f g (Kleisli h) = Kleisli (fmap g . h . f)
  {-# INLINE dimap #-}



class Profunctor p where
  {-# MINIMAL dimap | (lmap, rmap) #-}  
  -- | Map over both arguments at the same time.
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g
  {-# INLINE dimap #-}

  -- | Map the first argument contravariantly.
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  {-# INLINE lmap #-}

  -- | Map the second argument covariantly.
  rmap :: (b -> c) -> p a b -> p a c
  rmap = dimap id
  {-# INLINE rmap #-}  

  







-- | [NOTE Key lookup + value conversion, behaviour of row functions ]
--
-- If the key is not found /or/ the conversion fails, use a default; the first exception thrown by the lookup-and-convert function will be rethrown.
-- We'd like instead to try many different decoders, and only throw if /all/ have failed
-- 
-- How should Alternative behave for lookup-and-convert that both might fail?
-- 
-- value decoding : try all decoders, return first successful (== Alternative)
-- decoding missing values : should be configurable
-- -- * use default value
-- -- * skip rows that have any missing value
-- 
-- row function: decoding behaviour should be defined for all function arguments
--
-- example : 
--
-- λ> bool (2 :: Int) (VBool False) <|> bool (2 :: Int) (VBool True)
-- False
-- λ> bool (2 :: Int) (VDouble 32) <|> bool (2 :: Int) (VBool True)
-- *** Exception: ValueTypeError 2 VTypeBool (VDouble 32.0)
--
-- λ> Nothing <|> pure 32.0
-- Just 32.0
-- λ> (bool (2 :: Int) (VDouble 32) <|> bool (2 :: Int) (VBool True)) :: Maybe Bool
-- Just True
--
-- ^ throwM in IO : strict (the first failing decoder throws an exception), Maybe : lazy (keeps trying decoders and returns the first successful one)







-- | Pair of key and an extraction function.
data Arg m k v a = Arg k (v -> m a) deriving (Functor)

-- | Composable row lookup, based on the free applicative
newtype Decoder m k v a = Decoder (Ap (Arg m k v) a) deriving (Functor, Applicative)

-- | Lifts a single 'Arg' into a 'Decoder'
fromArg :: Arg m k v a -> Decoder m k v a
fromArg = Decoder . liftAp

-- | Simple 'Decoder' that just looks up and returns the value for a given key.
require :: Applicative m => k -> Decoder m k v v
require k = fromArg (Arg k pure)

-- | Shorthand for lookup and transform.
requireWhere :: k -> (k -> v -> m a) -> Decoder m k v a
requireWhere k e = fromArg (Arg k (e k))

-- | List all column names used in the 'Decoder'.
decoderKeys :: Key k => Decoder m k v a -> [k]
decoderKeys (Decoder x) = go x
  where
    go :: Ap (Arg m k v) a -> [k]
    go (Pure _)            = []
    go (Ap (Arg k _) rest) = k : go rest

-- This is pretty sensitive to let bindings
apRow :: (Key k, Monad m) => Ap (Arg m k v) a -> (k -> m v) -> m a
apRow (Pure a) _ = pure a
apRow (Ap (Arg k f) rest) row = do
  v <- row k
  z <- f v
  fz <- apRow rest row
  return (fz z)

-- | Run a 'Decoder' with a lookup function (typically row lookup).
runDecoder :: (Key k, Monad m) => Decoder m k v a -> (k -> m v) -> m a
runDecoder (Decoder x) = apRow x
