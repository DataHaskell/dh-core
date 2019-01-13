{-# language DeriveDataTypeable, DeriveGeneric #-}
{-# language GADTs, DataKinds, FlexibleContexts #-}
{-# language LambdaCase #-}
module Main where

import qualified Analyze.RFrame as AR
import qualified Analyze.Values as AV
import qualified Analyze.Values.Generic as AVG

import Analyze.Conversions (projectRow, projectRows)

import qualified GHC.Generics as G

-- | generics-sop
import Generics.SOP (Proxy(..), Generic, Code(..), from, unSOP, unZ, mapIK, hcmap, K(..), I(..), AllN(..), All(..), HAp(..), Prod(..), NP(..))
-- | sop-core
import Generics.SOP.NP (collapse_NP)

-- | typeable
import Data.Data (Typeable, Data(..), constrFields)

-- import Data.Hashable (Hashable(..))
-- import Control.Exception (Exception(..))
-- import Control.Monad.Catch (MonadThrow(..))

-- import qualified Data.Text as T
-- import qualified Data.Vector as V
-- import qualified Data.HashMap.Lazy as HM

import Numeric.Datasets (getDataset)
import Numeric.Datasets.Titanic

instance Generic TitanicEntry









-- experiments with generics
data Ta = Ca1 | Ca2 deriving (Eq, Show, Typeable, Data, G.Generic)
data Tb = Cb1 | Cb2 | Cb3 deriving (Eq, Show, Typeable, Data, G.Generic)
data P1 = P1 Int Char deriving (Eq, Show, Typeable, Data, G.Generic)
instance Generic P1
data P2 = P2 { p2i :: Int, p2c :: Char } deriving (Eq, Show, Typeable, Data, G.Generic)
instance Generic P2 
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


  



main = putStrLn "hello!"






