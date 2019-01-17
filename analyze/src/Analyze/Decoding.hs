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
  --   -- * Decoder based on a free alternative functor
  -- , DecAlt
  -- , fromArgA
  -- , requireA
  -- , requireWhereA
  ) where

import           Analyze.Common           (Key)

import           Control.Applicative      (Alternative(..))
-- import Control.Monad (MonadPlus(..))
import Data.Foldable (Foldable(..), asum)
import           Control.Applicative.Free (Ap(..), liftAp)
import qualified Control.Alternative.Free as L (Alt(..), liftAlt, AltF(..)) 
-- import           Data.Maybe               (fromMaybe)


import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
  




--

newtype P k a = P { runP :: k -> Maybe a } deriving Functor

instance Applicative (P k) where
  pure = pureP
  (<*>) = apP

pureP :: a -> P k a
pureP x = P $ \ _ -> Just x

apP :: P k (t -> a) -> P k t -> P k a
apP (P af) (P aa) = P $ \ k -> do
  f <- af k
  a <- aa k
  pure $ f a

instance Alternative (P k) where
  empty = emptyP
  (<|>) = altP

emptyP :: P k a
emptyP = P $ const Nothing

altP :: P k a -> P k a -> P k a
altP (P p) (P q) = P $ \k ->
  case p k of
    Nothing -> q k
    r -> r




--

newtype R k v m a = R { runR :: k -> v -> m a } deriving Functor

instance Applicative f => Applicative (R k v f) where
  pure  = pureR
  (<*>) = apR

pureR :: Applicative m => a -> R k v m a
pureR x = R $ \ _ _ -> pure x

apR :: Applicative f => R k v f (t -> a) -> R k v f t -> R k v f a
apR (R af) (R aa) = R $ \ k v -> af k v <*> aa k v

instance Alternative f => Alternative (R k v f) where
  empty = emptyR
  (<|>) = altR

emptyR :: Alternative f => R k v f a
emptyR = R $ \ _ _ -> empty

altR :: Alternative f => R k v f a -> R k v f a -> R k v f a
altR (R r1) (R r2) = R $ \ k v -> r1 k v <|> r2 k v 








-- newtype S k v m a = S { runS :: v -> m a } deriving Functor



-- -- pureR :: Applicative m => a -> R k v m a
-- pureS x = R $ \ _ -> pure x

-- -- apR :: Applicative f => R k v f (t -> a) -> R k v f t -> R k v f a
-- apS (S af) (S aa) = S $ \ k v -> af k <*> aa k

-- -- instance Alternative f => Alternative (S k v f) where
-- --   empty = emptyS
-- --   (<|>) = altS

-- -- emptyR :: Alternative f => R k v f a
-- -- emptyS = S $ \ _ -> empty

-- -- altR :: Alternative f => R k v f a -> R k v f a -> R k v f a
-- altS (S r1) (S r2) = S $ \ k -> r1 k <|> r2 k  





data S k v m a = S (Maybe k) (v -> m a) deriving Functor

instance Applicative f => Applicative (S k v f) where
  pure  = pureS
  (<*>) = apS

pureS :: Applicative m => a -> S k v m a
pureS x = S Nothing $ \ _ -> pure x

apS :: Applicative m => S k v m (a1 -> a2) -> S k v m a1 -> S k v m a2
apS (S k1 af) (S k2 aa) = S k3 $ \ v -> af v <*> aa v where
  k3 = k1 <|> k2

instance Alternative f => Alternative (S k v f) where
  empty = emptyS
  (<|>) = altS

emptyS :: Alternative f => S k v f a
emptyS = S Nothing $ const empty

altS :: Alternative f => S k v f a -> S k v f a -> S k v f a
altS (S k1 af) (S k2 aa) = S k3 $ \ v -> af v <|> aa v where
  k3 = k1 <|> k2     

decodeS :: (k -> v -> m a) -> k -> S k v m a
decodeS e k = S (Just k) (e k)

runS :: (Monad m, Alternative m) => S k v m a -> (k -> m v) -> m a
runS (S mk fun) row = do
  k <- case mk of
        Just k0 -> pure k0
        Nothing -> empty
  v <- row k
  fun v


liftS2 d1 d2 f k1 k2 = f <$> decodeS d1 k1 <*> decodeS d2 k2








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

decode :: (k -> v -> m a) -> k -> Decoder m k v a
decode = flip requireWhere

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
