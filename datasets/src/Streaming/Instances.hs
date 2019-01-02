{-# OPTIONS_GHC -Wno-orphans #-}
module Streaming.Instances () where

import Streaming (Stream, lift)
import Control.Monad.Catch (MonadThrow(throwM))

instance (Functor f, MonadThrow m) => MonadThrow (Stream f m) where
  throwM e = lift (throwM e)








