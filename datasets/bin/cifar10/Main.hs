module Main where

import Codec.Picture (writePng)
import Numeric.Datasets.Internal.Streaming (streamDataset)
import Numeric.Datasets.CIFAR10 (CIFARImage(..), cifar10)
import System.FilePath
import System.Directory (XdgDirectory(..), getXdgDirectory, createDirectoryIfMissing, listDirectory)
import qualified Streaming.Prelude as S
import System.ProgressBar (newProgressBar, defStyle, Progress(..), incProgress)

main :: IO ()
main = provision

-- may be required if you don't already have CIFAR10 loaded
provision :: IO ()
provision = do
  xdgCache <- getXdgDirectory XdgCache "datasets-hs"
  let imfolder = xdgCache </> "cifar-10-imagefolder"
  createDirectoryIfMissing True imfolder
  -- progress bar
  let n = 60000
  pb <- newProgressBar defStyle 10 (Progress 0 n ())
  let
    go :: FilePath -> CIFARImage -> IO ()
    go cachefolder (CIFARImage (im, lbl)) = do
      let labelfolder = cachefolder </> show lbl
      createDirectoryIfMissing True labelfolder
      ix <- length <$> listDirectory labelfolder
      let
        fname = show lbl ++ "_" ++ show ix ++ ".png"
      writePng (labelfolder </> fname) im
      incProgress pb 1 -- increment progress bar

  -- build the image folder
  S.mapM_ (go imfolder) $ streamDataset cifar10


