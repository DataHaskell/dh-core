{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Numeric.Datasets.Internal.Streaming
    ( streamDataset
    ) where

import Control.Exception.Safe
import Streaming (Stream, Of, lift)
import Data.ByteString.Lazy as B
import qualified Data.ByteString.Streaming as BS
import qualified Streaming         as S
import qualified Streaming.Cassava as S
import Control.Monad.Error.Class

import Numeric.Datasets

-- | Stream a ByteString into a Haskell value
streamDataset
  :: forall m a
  .  (MonadThrow m)
  => ReadAs a
  -> B.ByteString
  -> Stream (Of a) m ()
streamDataset ra bs = _streamDataset ra (BS.fromLazy bs)


-- private function which uses the streaming interface of bytestring
_streamDataset
  :: forall mt a r e
  .  (MonadThrow mt, Exception e)
  => (MonadError S.CsvParseException (Either e))
  => ReadAs a
  -> BS.ByteString (Either e) r
  -> Stream (Of a) mt r
_streamDataset ra bs =
  case ra of
    JSON -> lift $ throwString "Not implemented: JSON streaming"
    CSVRecord hhdr opts -> S.hoist monadErrorToThrow $ S.decodeWith opts hhdr bs
    CSVNamedRecord opts -> S.hoist monadErrorToThrow $ S.decodeByNameWith opts bs
  where
    monadErrorToThrow :: (Monad m, MonadThrow m) => (forall x e . Exception e => Either e x -> m x)
    monadErrorToThrow = \case
      Left e  -> throwM e
      Right e -> pure e



