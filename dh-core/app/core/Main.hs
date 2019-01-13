{-# language DeriveDataTypeable, DeriveGeneric #-}
{-# language GADTs, DataKinds, FlexibleContexts #-}
{-# language LambdaCase #-}
module Main where

import qualified Analyze.RFrame as AR
import Analyze.Conversions (projectRow, projectRows)

import qualified GHC.Generics as G

-- | generics-sop
import Generics.SOP (Proxy(..), Generic, Code(..), from, unSOP, unZ, mapIK, hcmap, K(..), I(..), AllN(..), All(..), HAp(..), Prod(..), NP(..))
-- | sop-core
-- import Data.SOP.NP (collapse_NP)

-- | typeable
import Data.Data (Typeable, Data(..), constrFields)

import Data.Hashable (Hashable(..))
import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow(..))

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM

import Numeric.Datasets (getDataset)
import Numeric.Datasets.Titanic

instance Generic TitanicEntry


-- | Computes an ordered hashmap of record constructor fields. Only works with records that have named constructors, e.g.
--
-- >>> data P1 = P1 Int Char 
-- >>> data P2 = P2 { p2i :: Int, p2c :: Char }
-- 
-- >>> > recFields $ P2 42 'z'
-- fromList [(0,"p2i"),(1,"p2c")]
--
-- >>> > recFields $ P1 42 'z'
-- *** Exception: Anonymous records not implemented yet
recFields :: (MonadThrow m, Eq k, Hashable k, Num k, Enum k, Data a) => 
             a -> m (HM.HashMap k T.Text)
recFields d | null constrs = throwM AnonRecordE
            | otherwise = pure $ HM.fromList $ zip [0 .. ] (T.pack `map` constrs) where
  constrs = constrFields (toConstr d)

toRFrame ds
  | null constrs = throwM AnonRecordE
  | otherwise = undefined -- AR.RFrame vc 
  where
    d = head ds
    cm = zip (T.pack `map` constrs) [0 ..]
    constrs = constrFields (toConstr d)
    vc = V.fromList constrs

data DataException = AnonRecordE deriving (Eq, Typeable)
instance Show DataException where
  show = \case
    AnonRecordE -> "Anonymous records not implemented yet"
instance Exception DataException



data Value = VInt Int | VDouble Double | VText T.Text | VChar Char deriving (Eq, Show)

class ToValue v where
  toValue :: v -> Value

instance ToValue Int where toValue = VInt
instance ToValue Double where toValue = VDouble
instance ToValue T.Text where toValue = VText
instance ToValue Char where toValue = VChar

-- | Python in Haskell yay!
npToValue :: (Generic a, All ToValue xs, Code a ~ '[xs]) => a -> [Value]
npToValue = collapse_NP . gMapToValue . gToList

-- | 
-- 
-- >>> gMapToValue :: (AllN (Prod h) ToValue xs, HAp h) => h I xs -> h (K Value) xs
-- >>> gMapToValue :: AllN (Prod NP) ToValue xs => NP I xs -> NP (K Value) xs
gMapToValue :: All ToValue xs => NP I xs -> NP (K Value) xs
gMapToValue d = hcmap (Proxy :: Proxy ToValue) (mapIK toValue) d

gToList :: (Generic a, Code a ~ '[x]) => a -> NP I x
gToList d = unZ $ unSOP (from d)

-- | This is here because generics-sop doesn't re-export a function with the same name that was recently moved into sop-core
collapse_NP :: NP (K a) xs -> [a]
collapse_NP Nil         = []
collapse_NP (K x :* xs) = x : collapse_NP xs



-- experiments with generics
data Ta = Ca1 | Ca2 deriving (Eq, Show, Typeable, Data, G.Generic)
data Tb = Cb1 | Cb2 | Cb3 deriving (Eq, Show, Typeable, Data, G.Generic)
data P1 = P1 Int Char deriving (Eq, Show, Typeable, Data)
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






