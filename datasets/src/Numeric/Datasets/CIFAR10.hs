-------------------------------------------------------------------------------
-- |
-- Module    :  Numeric.Datasets.CIFAR10
-- License   :  BSD-3-Clause
-- Stability :  experimental
-- Portability: non-portable
--
-- The binary version contains the files data_batch_1.bin, data_batch_2.bin,
-- ..., data_batch_5.bin, as well as test_batch.bin. Each of these files is
-- formatted as follows:
--
--     <1 x label><3072 x pixel>
--     ...
--     <1 x label><3072 x pixel>
--
-- In other words, the first byte is the label of the first image, which is a
-- number in the range 0-9. The next 3072 bytes are the values of the pixels of
-- the image. The first 1024 bytes are the red channel values, the next 1024
-- the green, and the final 1024 the blue. The values are stored in row-major
-- order, so the first 32 bytes are the red channel values of the first row of
-- the image.
-------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Numeric.Datasets.CIFAR10
  ( Label(..)
  , CIFARImage(..), height, width, image, label
  , cifarURL
  , cifar10
  , parseCifar
  ) where

import Codec.Picture (Image, PixelRGB8(PixelRGB8), Pixel8, writePixel)
import Codec.Picture.Types (newMutableImage, freezeImage)
import Control.Exception (throw)
-- import Control.Exception.Safe (throwM)
import Control.Monad.ST (runST)
import Data.List (zipWith4)
import GHC.Generics (Generic)
import Network.HTTP.Req (Url, (/:), https, Scheme(..))
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.Attoparsec.ByteString.Lazy as Atto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Numeric.Datasets

-- ========================================================================= --

-- | labels of CIFAR-10 dataset. Enum corresponds to binary-based uint8 label.
data Label
  = Airplane
  | Automobile
  | Bird
  | Cat
  | Deer
  | Dog
  | Frog
  | Horse
  | Ship
  | Truck
  deriving stock (Show, Eq, Generic, Bounded, Enum)

-- | Data representation of a CIFAR image is a 32x32 RGB image
newtype CIFARImage = CIFARImage { getXY :: (Image PixelRGB8, Label) }
  deriving newtype (Eq)

instance Show CIFARImage where
  show im = "CIFARImage{Height: 32, Width: 32, Pixel: RGB8, Label: " ++ show (label im) ++ "}"

-- | height of 'CIFARImage'
height :: Int
height = 32

-- | width of 'CIFARImage'
width :: Int
width = 32

-- | extract the JuicyPixel representation from a CIFAR datapoint
image :: CIFARImage -> Image PixelRGB8
image = fst . getXY

-- | extract the label from a CIFAR datapoint
label :: CIFARImage -> Label
label = snd . getXY

-- | Source URL for cifar-10 and cifar-100
cifarURL :: Url 'Https
cifarURL = https "www.cs.toronto.edu" /: "~kriz"

-------------------------------------------------------------------------------
tempdir :: Maybe FilePath
tempdir = Nothing

-- | Define a dataset from a source for a CSV file
cifar10 :: Dataset CIFARImage
cifar10 = Dataset
  (URL $ cifarURL /: "cifar-10-binary.tar.gz")
  tempdir
  (Just unzipCifar)
  (Parsable parseCifar)

-- cifar10Sha256 = "c4a38c50a1bc5f3a1c5537f2155ab9d68f9f25eb1ed8d9ddda3db29a59bca1dd"

-- | parser for a cifar binary
parseCifar :: Atto.Parser CIFARImage
parseCifar = do
  label :: Label <- toEnum . fromIntegral <$> Atto.anyWord8
  rs :: [Pixel8] <- BS.unpack <$> Atto.take 1024
  gs :: [Pixel8] <- BS.unpack <$> Atto.take 1024
  bs :: [Pixel8] <- BS.unpack <$> Atto.take 1024
  let ipixels = zipWith4 (\ix r g b -> (ix, PixelRGB8 r g b)) ixs rs gs bs
  pure $ CIFARImage (newImage ipixels, label)
  where
    newImage :: [((Int, Int), PixelRGB8)] -> Image PixelRGB8
    newImage ipixels = runST $ do
      mim <- newMutableImage height width
      mapM_ (\((x, y), rgb) -> writePixel mim x y rgb) ipixels
      freezeImage mim

    ixs :: [(Int, Int)]
    ixs = concat $ zipWith (\(row::Int) cols -> (row,) <$> cols) [0..] (replicate height [0..width - 1])

-- | how to unpack the tarball
--
-- FIXME: this should be in MonadThrow
unzipCifar :: BL.ByteString -> BL.ByteString
unzipCifar zipbs = do
  either (throw . fst) (BL.concat) $ Tar.foldlEntries go [] entries
  where
    entries :: Tar.Entries Tar.FormatError
    entries = Tar.read $ GZip.decompress zipbs

    go :: [BL.ByteString] -> Tar.Entry -> [BL.ByteString]
    go agg entry =
      case Tar.entryContent entry of
        Tar.NormalFile ps fs ->
          -- Each file is exactly 30730000 bytes long. All other files are metadata. See https://www.cs.toronto.edu/~kriz/cifar.html
          if fs == 30730000
          then ps:agg
          else agg
        _ -> agg

