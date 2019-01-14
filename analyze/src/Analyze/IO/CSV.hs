-- | Functions for working with CSV files.
module Analyze.IO.CSV where

import           Analyze.Conversions        (projectRows)
import           Analyze.RFrame             (RFrame (..), RFrameUpdate (..), empty, fromUpdate)
import           Control.Monad.Catch        (Exception, MonadThrow (..))
import qualified Data.Binary.Builder        as B
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Csv                   as C
import qualified Data.Csv.Builder           as CB
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.Typeable              (Typeable)
import qualified Data.Vector                as V

-- | Exception to wrap Cassava error strings.
data CsvError = CsvError String deriving (Eq, Show, Typeable)
instance Exception CsvError

-- | Decode CSV bytes as an 'RFrame' with a header row.
decodeWithHeader :: MonadThrow m => LBS.ByteString -> m (RFrame Text Text)
decodeWithHeader bs =
  case C.decodeByName bs of
    Left err -> throwM (CsvError err)
    Right (header, rows) -> do
      let ks = decodeUtf8 <$> header
      projectRows ks rows

-- | Decode CSV bytes as an 'RFrame' without a header row.
decodeWithoutHeader :: MonadThrow m => LBS.ByteString -> m (RFrame Int Text)
decodeWithoutHeader bs =
  case C.decode C.NoHeader bs of
    Left err -> throwM (CsvError err)
    Right rows ->
      if V.null rows
        then return empty
        else do
          let ks = V.imap const (V.head rows)
              update = RFrameUpdate ks rows
          fromUpdate update

-- | Encode an 'RFrame' as CSV bytes with a header row.
encodeWithHeader :: RFrame Text Text -> LBS.ByteString
encodeWithHeader (RFrame ks _ vs) =
  let header = CB.encodeHeader (encodeUtf8 <$> ks)
      rows = header `mappend` foldMap (CB.encodeRecord . (encodeUtf8 <$>)) vs
  in B.toLazyByteString header

-- | Encode an 'RFrame' as CSV bytes without header row.
encodeWithoutHeader :: RFrame k Text -> LBS.ByteString
encodeWithoutHeader (RFrame _ _ vs) =
  B.toLazyByteString (foldMap (CB.encodeRecord . (encodeUtf8 <$>)) vs)


loadCSVFileWithHeader :: FilePath -> IO (RFrame Text Text)
loadCSVFileWithHeader fileName = do
  contents <- readFile fileName
  decodeWithHeader (LBS8.pack contents)
  
loadCSVFileWithoutHeader :: FilePath -> IO (RFrame Int Text)
loadCSVFileWithoutHeader fileName = do
  contents <- readFile fileName
  decodeWithoutHeader (LBS8.pack contents)
  
