{-# OPTIONS_GHC -Wno-orphans  #-}
{-# LANGUAGE DataKinds #-}
module Numeric.Dataloader.Benchmark where

import Control.DeepSeq
import Control.Concurrent
import Data.Proxy
import Numeric.Dataloader
import Numeric.Datasets
import Numeric.Datasets.Abalone
import Streaming (Of)
import Network.HTTP.Req (Scheme(Http))
import qualified Streaming.Prelude as S
import qualified System.Random.MWC as MWC

import Criterion.Main

instance NFData Abalone
instance NFData Sex
instance (NFData a, NFData b) => NFData (Of a b)

mkDataset :: IO (Dataset 'Http Abalone)
mkDataset = pure abalone

mkDataloaderWithIx :: Dataset h a -> IO (Dataloader 1 h a a)
mkDataloaderWithIx ds = MWC.withSystemRandom $ \g -> do
  ixl <- uniformIxline ds g
  pure $ Dataloader Proxy (Just ixl) ds pure

main :: IO ()
main = do
  ds <- mkDataset
  dl <- mkDataloaderWithIx ds
  defaultMain
    [ bgroup "Numeric.Dataloader"
      [ bench "making an ixline" $ nfIO $ MWC.withSystemRandom (uniformIxline ds)
      , bgroup "testStream"
        [ bench "with ixline" . nfIO $ foldStream dl
        , bench "no ixline"   . nfIO $ foldStream (Dataloader Proxy Nothing ds pure)
        ]
      ]
    ]

slow :: S.Stream (Of a) IO r -> S.Stream (Of a) IO r
slow = S.mapM (\a -> threadDelay 2 >> pure a)

foldStream :: Show a => Dataloader 1 h a a -> IO (Of [a] ())
foldStream = S.fold (\memo a -> a:memo) [] id . slow . stream


