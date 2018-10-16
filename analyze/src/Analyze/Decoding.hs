{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Applicative decoding with key-value lookups.
--   Think of a 'Decoder' as a row function that exposes the columns it uses.
module Analyze.Decoding
  ( Arg (..)
  , Decoder(..)
  , decoderKeys
  , fromArg
  , require
  , requireWhere
  , runDecoder
  ) where

import           Analyze.Common           (Data)
import           Control.Applicative.Free (Ap (..), liftAp)
import           Data.Maybe               (fromMaybe)

-- | Pair of key and an extraction function.
data Arg m k v a = Arg k (v -> m a) deriving (Functor)

-- | Free applicative over 'Arg'.
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
decoderKeys :: Data k => Decoder m k v a -> [k]
decoderKeys (Decoder x) = go x
  where
    go :: Ap (Arg m k v) a -> [k]
    go (Pure _)            = []
    go (Ap (Arg k _) rest) = k : go rest

-- This is pretty sensitive to let bindings
apRow :: (Data k, Monad m) => Ap (Arg m k v) a -> (k -> m v) -> m a
apRow (Pure a) _ = pure a
apRow (Ap (Arg k f) rest) row = do
  v <- row k
  z <- f v
  fz <- apRow rest row
  return (fz z)

-- | Run a 'Decoder' with a lookup function (typically row lookup).
runDecoder :: (Data k, Monad m) => Decoder m k v a -> (k -> m v) -> m a
runDecoder (Decoder x) = apRow x
