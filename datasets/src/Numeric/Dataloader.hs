{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Numeric.Dataloader where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy(Proxy))
import Data.Sequence (Seq)
import Data.Vector (Vector)
import GHC.TypeLits (Nat, KnownNat, natVal)
import Streaming (Stream, Of(..))
import System.Random.MWC (GenIO)
import qualified Data.Vector          as V
import qualified Streaming            as S
import qualified Streaming.Prelude    as S
import qualified System.Random.MWC    as MWC
import qualified System.Random.MWC.Distributions as MWC
import Control.Exception.Safe (MonadThrow)
import Streaming.Instances ()

#ifdef __PARALLEL_HASKELL__
import Control.Concurrent.Async (mapConcurrently)
#endif

import Numeric.Datasets

-- * Configuring data loaders

-- | Options for a data loading functions.
data Dataloader bs h a = Dataloader
  { batchSizeProxy :: Proxy (bs :: Nat)
  , indexline :: Maybe (Vector Int)
  , dataset :: Dataset h a
  }

-- | Get the desired batch size from the type level.
getBatchSize :: (Integral i, KnownNat bs) => Dataloader bs h a -> i
getBatchSize = fromIntegral . natVal . batchSizeProxy

-- | Use a dataloader's indexline to return a permuted vector (or return the
-- identity vector).
permute :: Dataloader bs h a -> Vector a -> Vector a
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

-- | Stream a dataset.
--
-- NOTE: Run with @-threaded -rtsopts@ to concurrently load data in-memory.
stream
  :: forall a h io . (MonadThrow io, MonadIO io)
  => Dataloader 1 h a
  -> Stream (Of a) io ()
stream loader
  = permute loader <$> getDatavec (dataset loader)
#ifdef __PARALLEL_HASKELL__
  >>= liftIO . mapConcurrently pure
#endif
  >>= S.each

-- | Stream batches of a dataset.
--
-- NOTE: Run with @-threaded -rtsopts@ to concurrently load data in-memory.
batchStream
  :: (MonadThrow io, MonadIO io, KnownNat bs)
  => Dataloader bs h a
  -> Stream (Of (Seq a)) io ()
batchStream dl
  = S.slidingWindow (getBatchSize dl)
  $ stream (dl { batchSizeProxy = Proxy @1 })


