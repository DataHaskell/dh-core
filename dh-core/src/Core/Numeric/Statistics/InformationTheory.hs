module Core.Numeric.Statistics.InformationTheory where

import Core.Data.Dataset

-- | Differential entropy has a singularity at 0 but converges slowly to 0 for small positive values.
entropyR :: (Foldable t, Ord h, Floating h) => Dataset k (t a) -> h
entropyR = entropyR_ . probClasses

entropyR_ :: (Foldable t, Functor t, Ord c, Floating c) => t c -> c
entropyR_ ps = negate . sum $ entropyReg <$> ps where
  entropyReg p | p > 0 =  p * logBase 2 p
               | otherwise = 0


-- | Gini index (expected error rate)
gini :: (Foldable t, Floating c) => Dataset k (t a) -> c
gini = gini_ . probClasses

gini_ :: (Foldable t, Functor t, Floating c) => t c -> c
gini_ ps = 1 - sum ((**2) <$> ps)
