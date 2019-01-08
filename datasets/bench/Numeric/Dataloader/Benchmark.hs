{-# OPTIONS_GHC -Wno-orphans  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module Numeric.Dataloader.Benchmark where

import Control.DeepSeq
import Control.Concurrent
import Numeric.Dataloader
import Numeric.Datasets
import Numeric.Datasets.Abalone
import Streaming (Of)
import qualified Streaming.Prelude as S
import qualified System.Random.MWC as MWC

-- ImageLoading bench
import Numeric.Datasets.Internal.Streaming
import Numeric.Datasets.CIFAR10
import System.FilePath
import System.Directory
import Codec.Picture
import Control.Exception.Safe
import Text.Read
import qualified Data.List.NonEmpty   as NonEmpty

import Criterion.Main

instance NFData Abalone
instance NFData Sex
instance (NFData a, NFData b) => NFData (Of a b)

mkDataset :: IO (Dataset Abalone)
mkDataset = pure abalone

mkDataloaderWithIx :: Dataset a -> IO (Dataloader a a)
mkDataloaderWithIx ds = MWC.withSystemRandom $ \g -> do
  ixl <- uniformIxline ds g
  pure $ Dataloader 1 (Just ixl) ds pure

main :: IO ()
main = do
  ds <- mkDataset
  dl <- mkDataloaderWithIx ds
  cifar10l <- cifar10ImageLoader
  defaultMain
    [ bgroup "Numeric.Dataloader"
      [ bench "making an ixline" $ nfIO $ MWC.withSystemRandom (uniformIxline ds)
      , bgroup "testStream"
        [ bench "with ixline" . nfIO $ foldStream (S.take 100 $ slow . stream $ dl)
        , bench "no ixline"   . nfIO $ foldStream (S.take 100 $ slow . stream $ Dataloader 1 Nothing ds pure)
        ]
      , bench "cifar10 image folder" $ nfIO $ foldStream $ S.take 1000 $ stream cifar10l
      , bench "cifar10 batch folder" $ nfIO $ foldStream $ S.take 100  $ batchStream (cifar10l { batchSize = 10 })
      ]
    ]

slow :: S.Stream (Of a) IO r -> S.Stream (Of a) IO r
slow = S.mapM (\a -> threadDelay 2 >> pure a)

foldStream :: Show a => S.Stream (Of a) IO () -> IO (Of [a] ())
foldStream = S.fold (\memo a -> a:memo) [] id

-------------------------------------------------------------------------------
-- Image Folder loading

-- may be required if you don't already have CIFAR10 loaded
provision :: IO ()
provision = do
  xdgCache <- getXdgDirectory XdgCache "datasets-hs"
  let imfolder = xdgCache </> "cifar-10-imagefolder"
  createDirectoryIfMissing True imfolder

  -- build the image folder
  S.mapM_ (go imfolder) $ streamDataset cifar10
 where
  go :: FilePath -> CIFARImage -> IO ()
  go cachefolder (CIFARImage (im, lbl)) = do
    let labelfolder = cachefolder </> show lbl
    createDirectoryIfMissing True labelfolder
    ix <- length <$> listDirectory labelfolder
    writePng (labelfolder </> (show lbl ++ "_" ++ show ix ++ ".png")) im


-- | dataloading the image folder dataset
cifar10ImageLoader :: IO (Dataloader (String, FilePath) CIFARImage)
cifar10ImageLoader = do
  xdgCache <- getXdgDirectory XdgCache "datasets-hs"
  let imfolder = xdgCache </> "cifar-10-imagefolder"
  pure $ Dataloader 1 Nothing (imgFolderDataset imfolder) load

 where
  labelFolders :: NonEmpty.NonEmpty String
  labelFolders = show <$> NonEmpty.fromList [minBound..maxBound::Label]

  imgFolderDataset :: FilePath -> Dataset (String, FilePath)
  imgFolderDataset fp =
    Dataset
      (ImgFolder fp labelFolders)
      Nothing
      Nothing
      (ImageFolder labelFolders)

  load :: (String, FilePath) -> IO CIFARImage
  load (lbl, fp) = CIFARImage <$> ((,) <$> imgIO <*> lblIO)
    where
      lblIO :: IO Label
      lblIO = either throwString pure $ readEither lbl

      imgIO :: IO (Image PixelRGB8)
      imgIO = readPng fp >>= \case
        Left err -> throwString err
        Right (ImageRGB8 i) -> pure i
        Right _ -> throwString "decoded image was not rgb8"

