{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Streaming.Instances () where

import Streaming (Stream, lift)
import Data.Attoparsec.ByteString.Streaming (Errors)
import Control.Monad.Catch (MonadThrow(throwM), Exception)

instance (Functor f, MonadThrow m) => MonadThrow (Stream f m) where
  throwM e = lift (throwM e)

instance Exception Errors where








