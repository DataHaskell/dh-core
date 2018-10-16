-- | Functions for working with HTML.
module Analyze.Html where

import           Analyze.RFrame (RFrame (..))
import           Control.Monad  (forM_)
import qualified Lucid          as L

-- | Renders an 'RFrame' to an HTML table.
renderHtml :: (L.ToHtml k, L.ToHtml v, Monad m)
           => RFrame k v -> L.HtmlT m ()
renderHtml (RFrame ks _ vs) =
  L.table_ $ do
    L.thead_ $
      L.tr_ $ forM_ ks (L.th_ . L.toHtml)
    L.tbody_ $ forM_ vs $ \v -> L.tr_ (forM_ v (L.td_ . L.toHtml))
