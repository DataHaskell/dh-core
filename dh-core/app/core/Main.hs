{-# language DeriveDataTypeable, DeriveGeneric #-}
module Main where

import qualified Analyze.RFrame as AR
import Analyze.Conversions (projectRow, projectRows)



import qualified GHC.Generics as G

-- | generics-sop
import Generics.SOP
-- | sop-core

-- | typeable
-- import Data.Typeable (Typeable(..), typeOf)
import Data.Data

import Data.Hashable (Hashable(..))
import Control.Monad.Catch (MonadThrow(..))

import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM

import Numeric.Datasets (getDataset)
import Numeric.Datasets.Titanic

instance Generic TitanicEntry



-- experiments with generics
data Ta = Ca1 | Ca2 deriving (Eq, Show, Typeable, Data, G.Generic)
data Tb = Cb1 | Cb2 | Cb3 deriving (Eq, Show, Typeable, Data, G.Generic)
data T = T Ta Ta Tb (Maybe Int) deriving (Eq, Show, Typeable, Data, G.Generic)

instance Generic Ta
instance Generic Tb
instance Generic T

-- | >>> from t0
-- SOP (Z (I Ca1 :* I Ca2 :* I Cb1 :* I (Just 42) :* Nil))
t0 = T Ca1 Ca2 Cb1 (Just 42)


newtype Tf f = Tf (f Int) deriving (G.Generic)
instance Generic (Tf f)

tf0 = Tf (Just 42) 

-- newtype Identity a = Identity a deriving (Show, G.Generic)
-- instance Generic (Identity a)



data A = A Int Char deriving (Eq, Show, G.Generic)
instance Generic A

fA :: (Int -> a) -> (Char -> b) -> A -> (a, b)
fA f1 f2 (A i c) = (f1 i, f2 c)

a0 = A 42 'z'


  



main = print "hello!"

