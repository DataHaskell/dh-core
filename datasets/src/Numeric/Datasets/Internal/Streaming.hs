{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-name-shadowing #-}
module Numeric.Datasets.Internal.Streaming
    ( streamDataset
    , streamByteString
    ) where

import Control.Exception.Safe (MonadThrow, Exception, throwString, throwM)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Attoparsec.ByteString.Lazy (Parser)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8)
import Data.List.NonEmpty (NonEmpty, toList)
import Streaming (Stream, Of((:>)), lift)
import qualified Data.ByteString.Streaming as BS (fromLazy, ByteString, null)
import qualified Data.ByteString      as B' (pack)
import qualified Data.ByteString.Lazy as B (ByteString, concat)
import qualified Data.List as L (intercalate)
import qualified Data.Attoparsec.ByteString.Streaming as Atto (parse)
import qualified Data.Attoparsec.ByteString.Lazy as Atto (anyWord8)
import qualified Streaming         as S (hoist, unfold)
import qualified Streaming.Cassava as S (decodeWith, decodeByNameWith, CsvParseException)
import qualified Streaming.Prelude as S (print, maps)

import Numeric.Datasets
import Streaming.Instances ()

-- | Stream a dataset
streamDataset
  :: forall io a . (MonadThrow io, MonadIO io)
  => Dataset a
  -> Stream (Of a) io ()
streamDataset ds = do
  folder <- liftIO $ defaultTempDir (temporaryDirectory ds)
  files  <- liftIO $ getFileFromSource folder (source ds)
  streamByteString (readAs ds) (fromMaybe id (preProcess ds) <$> files)


-- | Stream a ByteString into a Haskell value
streamByteString
  :: forall m a
  .  (MonadThrow m)
  => ReadAs a
  -> NonEmpty B.ByteString
  -> Stream (Of a) m ()
streamByteString ra bs = _streamDataset ra (BS.fromLazy $ B.concat $ toList bs)


-- private function which uses the streaming interface of bytestring
_streamDataset
  :: forall mt a e
  .  (MonadThrow mt, Exception e)
  => (MonadError S.CsvParseException (Either e))
  => ReadAs a
  -> BS.ByteString (Either e) ()
  -> Stream (Of a) mt ()
_streamDataset ra bs =
  case ra of
    JSON -> lift $ throwString "Not implemented: JSON streaming"
    CSVRecord hhdr opts -> S.hoist either2Throw $ S.decodeWith opts hhdr bs
    CSVNamedRecord opts -> S.hoist either2Throw $ S.decodeByNameWith opts bs
    Parsable psr -> parseStream psr (S.hoist either2Throw bs)
    ImageFolder _ -> lift $ throwString "Not implemented: Image Folder streaming, use Dataloader"
  where
    either2Throw :: MonadThrow m => (forall x e . Exception e => Either e x -> m x)
    either2Throw = \case
      Left e -> throwM e
      Right r -> pure r


-- private function to generate a stream from a parser
parseStream
  :: forall m a . MonadThrow m => Parser a -> BS.ByteString m () -> Stream (Of a) m ()
parseStream psr = S.unfold go
  where
    go :: BS.ByteString m () -> m (Either () (Of a (BS.ByteString m ())))
    go bs = do
      (eea, rest) <- Atto.parse psr bs
      BS.null rest >>= \(empty :> _) ->
        if empty
        then pure $ Left ()
        else case eea of
          Left (es, lst) -> throwString (lst ++ "\n" ++ L.intercalate "\n" es)
          Right a -> pure $ Right (a :> rest)


-- make this a real test
test :: IO ()
test = do
  S.print $ S.maps render $ parseStream Atto.anyWord8 (BS.fromLazy "1")
  S.print $ S.maps render $ parseStream Atto.anyWord8 (BS.fromLazy "1 2 3 4")
  where
    render (a:>b) = (decodeUtf8 (B'.pack [a]) :> b)

