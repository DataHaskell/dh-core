-------------------------------------------------------------------------------
-- |
-- Module    :  Numeric.Dataloader
-- Stability :  experimental
-- Portability: non-portable
--
-- A Dataloader is an extension of a Dataset and is primarily intended for
-- compute-intensive, batch loading interfaces. When used with ImageFolder
-- representations of Datasets, it shuffles the order of files to be loaded
-- and leverages the async library when possible.
--
-- Concurrent loading primarily takes place in 'batchStream'. 'stream' exists
-- primarily to provide a unified API with training that is not batch-oriented.
-------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Dataloader
  ( Dataloader(..)
  , uniformIxline

  , stream
  , batchStream
  ) where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Vector (Vector)
import Streaming (Stream, Of(..))
import System.Random.MWC (GenIO)
import qualified Data.Vector          as V
import qualified Streaming            as S
import qualified Streaming.Prelude    as S
import qualified System.Random.MWC.Distributions as MWC
import Control.Exception.Safe (MonadThrow)
import Streaming.Instances ()

import Control.Parallel.Strategies

import Numeric.Datasets

-- * Configuring data loaders


-- | Options for a data loading functions.
data Dataloader a b = Dataloader
  { batchSize :: Int                 -- ^ Batch size used with 'batchStream'.
  , shuffle :: Maybe (Vector Int)    -- ^ Optional shuffle order (forces the dataset to be loaded in memory if it wasn't already).
  , dataset :: Dataset a             -- ^ Dataset associated with the dataloader.
  , transform :: a -> b              -- ^ Transformation associated with the dataloader which will be run in parallel. If using an
                                     --   ImageFolder, this is where you would transform image filepaths to an image (or other
                                     --   compute-optimized form). Additionally, this is where you should perform any
                                     --   static normalization.
  }


-- | Generate a uniformly random index line from a dataset and a generator.
uniformIxline
  :: Dataset a
  -> GenIO
  -> IO (Vector Int)
uniformIxline ds gen = do
  len <- V.length <$> getDatavec ds
  MWC.uniformPermutation len gen

-- * Data loading functions


-- | Stream a dataset in-memory, applying a transformation function.
stream
  :: (MonadThrow io, MonadIO io)
  => Dataloader a b
  -> Stream (Of b) io ()
stream dl = S.maps (\(a:>b) -> (transform dl a `using` rpar) :> b) (sourceStream dl)


-- | Stream batches of a dataset, concurrently processing each element
--
-- NOTE: Run with @-threaded -rtsopts@ to concurrently load data in-memory.
batchStream
  :: (MonadThrow io, MonadIO io, NFData b)
  => Dataloader a b
  -> Stream (Of [b]) io ()
batchStream dl
  = S.mapsM (S.toList >=> liftIO . firstOfM go)
  $ S.chunksOf (batchSize dl)
  $ sourceStream dl
  where
    go as = fmap (transform dl) as `usingIO` parList rdeepseq


-- * helper functions (not for export)


-- | Stream a dataset in-memory
sourceStream
  :: (MonadThrow io, MonadIO io)
  => Dataloader a b
  -> Stream (Of a) io ()
sourceStream loader
  = permute loader <$> getDatavec (dataset loader)
  >>= S.each
  where
    -- Use a dataloader's shuffle order to return a permuted vector (or return the
    -- identity vector).
    permute :: Dataloader a b -> Vector a -> Vector a
    permute loader va = maybe va (V.backpermute va) (shuffle loader)


-- | Monadic, concrete version of Control.Arrow.first for @Of@
firstOfM :: Monad m => (a -> m b) -> Of a c -> m (Of b c)
firstOfM fm (a:>c) = do
  b <- fm a
  pure (b:>c)



