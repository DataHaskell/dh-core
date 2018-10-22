module Core.Numeric.Statistics.Classification.Exceptions where

import Control.Exception
import Data.Typeable


-- * Exceptions

data ValueException = ZeroProbabilityE String deriving (Eq, Show, Typeable)

instance Exception ValueException 


data DataException =
    -- MissingFeatureE i
  IndexOobE String Int Int Int 
  | DimMismatchE String Int Int 
  deriving (Eq, Typeable)

instance Show DataException where
  show e = case e of
    IndexOobE errMsg ix blo bhi -> unwords [errMsg, ": index", show ix,"out of bounds", show (blo, bhi)]
    DimMismatchE errMsg d1 d2 -> unwords [errMsg, ": dimension mismatch : expecting", show d1, "but got", show d2]

instance Exception DataException
