{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Numeric.Dataloader
  ( Dataloader(..)
  , permute
  , uniformIxline
  , stream
  , batchStream
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Sequence (Seq)
import Data.Vector (Vector)
import Streaming (Stream, Of(..))
import System.Random.MWC (GenIO)
import qualified Data.Vector          as V
import qualified Streaming            as S
import qualified Streaming.Prelude    as S
import qualified System.Random.MWC.Distributions as MWC
import Control.Exception.Safe (MonadThrow)
import Streaming.Instances ()

import Control.Concurrent.Async (mapConcurrently)

import Numeric.Datasets

-- * Configuring data loaders

-- | Options for a data loading functions.
data Dataloader h a b = Dataloader
  { batchSize :: Int
  , indexline :: Maybe (Vector Int)
  , dataset :: Dataset h a
  , transformIO :: a -> IO b
  }

-- | Use a dataloader's indexline to return a permuted vector (or return the
-- identity vector).
permute :: Dataloader h a b -> Vector a -> Vector a
permute loader va = maybe va (V.backpermute va) (indexline loader)

-- | Generate a uniformly random index line from a dataset and a generator.
uniformIxline
  :: Dataset h a
  -> GenIO
  -> IO (Vector Int)
uniformIxline ds gen = do
  len <- V.length <$> getDatavec ds
  MWC.uniformPermutation len gen

-- * Data loading functions

-- | Stream a dataset in-memory, applying a transformation function, using concurrency where possible
stream
  :: forall a b h io . (MonadThrow io, MonadIO io)
  => Dataloader h a b
  -> Stream (Of b) io ()
stream dl = S.mapsM (liftIO . firstOfM (transformIO dl)) (sourceStream dl)

-- | Stream batches of a dataset, concurrently processing each element
--
-- NOTE: Run with @-threaded -rtsopts@ to concurrently load data in-memory.
batchStream
  :: (MonadThrow io, MonadIO io)
  => Dataloader h a b
  -> Stream (Of (Seq b)) io ()
batchStream dl
  = S.mapsM (liftIO . firstOfM (mapConcurrently (transformIO dl)))
  $ S.slidingWindow (batchSize dl)
  $ sourceStream dl


-- * helper functions (not for export)

-- | Stream a dataset in-memory
sourceStream
  :: (MonadThrow io, MonadIO io)
  => Dataloader h a b
  -> Stream (Of a) io ()
sourceStream loader
  = permute loader <$> getDatavec (dataset loader)
  >>= S.each

-- | Monadic, concrete version of Control.Arrow.first for @Of@
firstOfM :: Monad m => (a -> m b) -> Of a c -> m (Of b c)
firstOfM fm (a:>c) = do
  b <- fm a
  pure (b:>c)

