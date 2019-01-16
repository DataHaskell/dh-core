{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Applicative decoding with key-value lookups.
--   Think of a 'Decoder' as a row function that exposes the columns it uses.
module Analyze.Decoding
  (
    -- * Decoder
    Decoder
    -- ** Construction
  , require
  , requireWhere
  -- ** Execution
  , runDecoder
  -- ** Utilities
  , decoderKeys
    -- * Decoder based on a free alternative functor
  , DecAlt
  , fromArgA
  , requireA
  , requireWhereA
  ) where

import           Analyze.Common           (Key)
import           Control.Applicative      (Alternative(..))
import           Control.Applicative.Free (Ap(..), liftAp)
import qualified Control.Alternative.Free as L (Alt(..), liftAlt, AltF(..)) 
-- import           Data.Maybe               (fromMaybe)

-- | Composable row lookup, based on the free /alternative/ functor.
--
-- The idea is that 'DecAlt' decoders can be composed with <|>, so multiple ones can be tried in sequence until one succeeds (or the list is exhausted).
newtype DecAlt m k v a = DecAlt (L.Alt (Arg m k v) a) deriving (Functor, Applicative, Alternative)

fromArgA :: Arg m k v a -> DecAlt m k v a
fromArgA = DecAlt . L.liftAlt

requireA :: Applicative m => k -> DecAlt m k a a
requireA k = fromArgA $ Arg k pure

requireWhereA :: k -> (k -> v -> m a) -> DecAlt m k v a
requireWhereA k e = fromArgA $ Arg k (e k)

-- altRow (L.Alt (L.Pure f : as)) = undefined

-- altRow (L.Alt alt) rowf = case alt of
--   L.Ap fx ff : rest -> undefined
--   L.Pure f   : rest -> f rowf

-- runDecAlt (DecAlt da) = altRow da


-- | Pair of key and an extraction function.
data Arg m k v a = Arg k (v -> m a) deriving (Functor)

-- | Composable row lookup, based on the free applicative
newtype Decoder m k v a = Decoder (Ap (Arg m k v) a) deriving (Functor, Applicative)

-- instance Monad (Decoder m k v)

-- bindDecoder :: Decoder m k v a -> (a -> Decoder m k v b) -> Decoder m k v b

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
