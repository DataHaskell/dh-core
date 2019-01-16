-- | Various data-sciencey functions.
module Analyze.Ops
  ( oneHot
  ) where

import           Analyze.Common      (Key)
import           Analyze.Frame.Dense (RFrame (..), RFrameUpdate (..), col, splitCols, update)
import           Control.Monad.Catch (MonadThrow (..))
import qualified Data.HashSet        as HS
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

uniq :: Key k => Vector k -> Vector k
uniq ks = V.reverse (V.fromList newKsR)
  where
    acc (hs, uks) k =
      if HS.member k hs
        then (hs, uks)
        else (HS.insert k hs, k:uks)
    (_, newKsR) = V.foldl acc (HS.empty, []) ks

match :: Eq k => Vector k -> v -> v -> k -> Vector v
match ks yesVal noVal tk = V.map (\k -> if k == tk then yesVal else noVal) ks

-- | One-hot encode a given column. (See tests for usage.)
oneHot :: (Key k, MonadThrow m) => (k -> v -> k) -> k -> v -> v -> RFrame k v -> m (RFrame k v)
oneHot combine key yesVal noVal frame = do
  let (target, cold) = splitCols (== key) frame
  rawVs <- col key target
  let cookedKs = V.map (combine key) rawVs
      newKs = uniq cookedKs
      newVs = V.map (match newKs yesVal noVal) cookedKs
      hot = RFrameUpdate newKs newVs
  update hot cold
