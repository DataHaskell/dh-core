{-# language
    DeriveGeneric
  , DataKinds
  , FlexibleContexts
  , GADTs 
  , LambdaCase 
  , DeriveDataTypeable
  , TypeOperators
  , DefaultSignatures
  , ScopedTypeVariables 
#-}
{-# OPTIONS_GHC -Wall #-}
module Core.Data.Frame.Generic (
  gToTable,
  -- * DEBUG
  gToRow, gToRowMaybe
  ) where

import Data.Char (toLower)
import Generics.SOP hiding (fromList) -- (Generic(..), All, Code)
import Generics.SOP.GGP   (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)
-- import Generics.SOP.NP
import qualified GHC.Generics as G
import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow(..))
import Data.Data (Data(..), constrFields)
import Data.Typeable (Typeable)

import qualified Data.Foldable as F (Foldable(..)) 
import qualified Data.Text as T
-- import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable(..))

import Core.Data.Frame (Row, Frame, fromKVs, fromList, mkRow)
import qualified Core.Data.Frame.Value as AV
import qualified Core.Data.Frame.Value.Generic as AVG
-- import Core.Data.Frame.Generic (DataException(..))

-- $setup
-- >>> :set -XDeriveDataTypeable
-- >>> :set -XDeriveGeneric
-- >>> import Generics.SOP (Generic(..), All, Code)
-- >>> import Generics.SOP.NP
-- >>> import qualified GHC.Generics as G
-- >>> import Data.Data (Typeable, Data(..), constrFields)
-- >>> data P1 = P1 Int Char deriving (Eq, Show, Data, G.Generic)
-- >>> instance Generic P1
-- >>> data P2 = P2 { p2i :: Int, p2c :: Char } deriving (Eq, Show, Data, G.Generic)
-- >>> instance Generic P2
-- >>> data Q = Q { q1 :: Maybe Int, q2 :: Either Double Char } deriving (Eq, Show, Data, G.Generic)
-- >>> instance Generic Q

-- | Populate a 'Frame' using the rows' 'Data', 'G.Generic' and 'Generic' instances and throws a 'DataException' if the input data is malformed.
--
-- For example, a list of records having two fields each will produce a dataframe with two columns, having the record field names as column labels.
--
-- All record fields in the input data must be of types that are instances of the 'AV.ToValueM' class (as prescribed by the @All ToValueM xs@ constraint).
-- 
-- This function only accepts record types that have named constructor fields, such as P2 and Q in the following code :
--
-- @
-- data P1 = P1 Int Char deriving (Eq, Show, Data, G.Generic)
-- instance Generic P1
-- 
-- data P2 = P2 { p2i :: Int, p2c :: Char } deriving (Eq, Show, Data, G.Generic)
-- instance Generic P2
--
-- data Q = Q { q1 :: Maybe Int, q2 :: Either Double Char } deriving (Eq, Show, Data, G.Generic)
-- instance Generic Q
-- @
--
-- >>> gToTable [P1 42 'z']
-- *** Exception: Anonymous records not implemented yet
-- >>> gToTable [P2 42 'z']
-- Frame {tableRows = [("p2i",VInt 42),("p2c",VChar 'z')] :| []}
--
-- Test using 'Maybe' and 'Either' record fields :
--
-- >>> gToTable [Q (Just 42) (Left 1.2), Q (Just 1) (Right 'a'), Q Nothing (Right 'b')]
-- Frame {tableRows = [("q2",VDouble 1.2),("q1",VInt 42)] :| [[("q2",VChar 'a'),("q1",VInt 1)],[("q2",VChar 'b')]]}
--
-- As can be seen 'Nothing' values are not inserted in the rows, which can be used to encode missing data features.
gToTable :: (Code a ~ '[xs], All AV.ToValueM xs, Data a, Generic a, MonadThrow m) =>
             [a]
          -> m (Frame (Row T.Text AV.Value))
gToTable ds
  | null ds = throwM NoDataE 
  | otherwise = do
      constrs <- check (head ds)
      pure $ fromList $ map (gToRowMaybe constrs) ds

check :: (Data a, MonadThrow m) => a -> m [T.Text]
check d | null constrs = throwM AnonRecordE
        | otherwise = pure constrs where
  constrs = T.pack `map` constrFields (toConstr d)      


-- | Populate a 'Row' using the rows' 'Data', 'G.Generic' and 'Generic' instances 
gToRow :: (Code a ~ '[xs], Data a, Generic a, All AV.ToValue xs) =>
          [T.Text]
       -> a
       -> Row T.Text AV.Value
gToRow constrs d = fromKVs $ zip constrs (AVG.npToValue d)

-- | Populate a 'Row' using the rows' 'Data', 'G.Generic' and 'Generic' instances
--
-- NB The ToValueM currently supports only "shallow" Maybe and Either record fields, i.e. their fields must be concrete types (as in, @Either Int String@ or @Maybe Char@).
gToRowMaybe :: (Code a ~ '[xs], Data a, Generic a, All AV.ToValueM xs) =>
               [T.Text]
            -> a
            -> Row T.Text AV.Value
gToRowMaybe constrs d = mkRow (insertsMaybe kvs) where
  kvs = zip constrs (AVG.npToValueM d)



-- | Exceptions related to the input data
data DataException =
  AnonRecordE  -- ^ Anonymous records not implemented yet
  | NoDataE    -- ^ Dataset has 0 rows
  deriving (Eq, Typeable)
instance Show DataException where
  show = \case
    AnonRecordE -> "Anonymous records not implemented yet"
    NoDataE -> "The dataset has 0 rows"
instance Exception DataException


insertsMaybe :: (Foldable t, Eq k, Hashable k) =>
                t (k, Maybe v)
             -> HM.HashMap k v
insertsMaybe = F.foldl insf HM.empty where
  insf acc (k, vmay) = case vmay of
    Just v  -> HM.insert k v acc
    Nothing -> acc










-- test data

-- λ> datatypeInfo (Proxy :: Proxy A)
-- ADT "Core.Data.Frame.Generic" "A" (Constructor "A" :* Nil)
data A = A Int Char deriving (Eq, Show, G.Generic)
instance Generic A
instance HasDatatypeInfo A

-- ADT "Core.Data.Frame.Generic" "B" (Record "B" (FieldInfo "b1" :* FieldInfo "b2" :* Nil) :* Nil)
data B = B { b1 :: Int, b2 :: Char } deriving (Eq, Show, G.Generic)
instance Generic B
instance HasDatatypeInfo B

-- ADT "Core.Data.Frame.Generic" "C" (Constructor "C1" :* Constructor "C2" :* Constructor "C3" :* Nil)
data C = C1 | C2 | C3 deriving (Eq, Show, G.Generic)
instance Generic C
instance HasDatatypeInfo C

-- ADT "Core.Data.Frame.Generic" "D" (Constructor "D1" :* Constructor "D2" :* Constructor "D3" :* Nil)
data D = D1 Int | D2 Char | D3 (Maybe Int) deriving (Eq, Show, G.Generic)
instance Generic D
instance HasDatatypeInfo D

-- ADT "Core.Data.Frame.Generic" "E" (Record "E1" (FieldInfo "e1" :* Nil) :* Record "E2" (FieldInfo "e2" :* Nil) :* Record "E3" (FieldInfo "e3" :* Nil) :* Nil)
data E = E1 { e1 :: Int } | E2 { e2 :: Char } | E3 { e3 :: Maybe Int } deriving (Eq, Show, G.Generic)
instance Generic E
instance HasDatatypeInfo E

-- Newtype "Core.Data.Frame.Generic" "F" (Constructor "F")
newtype F = F (Either Int Char) deriving (Eq, Show, G.Generic)
instance Generic F
instance HasDatatypeInfo F

-- Newtype "Core.Data.Frame.Generic" "G" (Record "G" (FieldInfo "g" :* Nil))
newtype G = G { g :: Either Int (Maybe Char) } deriving (Eq, Show, G.Generic)
instance Generic G
instance HasDatatypeInfo G

-- ADT "Core.Data.Frame.Generic" ":@" (Infix ":+" LeftAssociative 9 :* Infix ":-" LeftAssociative 9 :* Nil)
data a :@ b = a :+ b | a :- b deriving (Eq, Show, G.Generic)
instance Generic (a :@ b)
instance HasDatatypeInfo (a :@ b)

data H = H1 C deriving (Eq, Show, G.Generic)
instance Generic H
instance HasDatatypeInfo H



-- λ> sopToVal (datatypeInfo (Proxy :: Proxy C)) (from C2)
-- Constr "C2" []
-- λ> sopToVal (datatypeInfo (Proxy :: Proxy B)) (from $ B 42 'x')
-- Rec (fromList [("b1",VInt 42),("b2",VChar 'x')])
-- λ> sopToVal (datatypeInfo (Proxy :: Proxy A)) (from $ A 42 'x')
-- Constr "A" [VInt 42,VChar 'x']
-- λ> sopToVal (datatypeInfo (Proxy :: Proxy (Maybe Int))) (from $ Just 42)
-- Constr "Just" [VInt 42]

sopToVal :: (All2 ToVal xss) => DatatypeInfo xss -> SOP I xss -> Val
sopToVal di (SOP xss) = hcollapse $ hcliftA2
    (Proxy :: Proxy (All ToVal))
    (\ci xs -> K (baz ci xs))
    (constructorInfo di)
    xss

baz :: All ToVal xs => ConstructorInfo xs -> NP I xs -> Val
baz (Constructor cn) xs = Constr cn $ hcollapse $
    hcmap (Proxy :: Proxy ToVal) (mapIK toVal) xs
baz (Record _ fi) xs = Rec $ M.fromList $ hcollapse $ hcliftA2 (Proxy :: Proxy ToVal) mk fi xs
  where
    mk :: ToVal v => FieldInfo v -> I v -> K (FieldName, Val) v
    mk (FieldInfo n) (I x) = K (n, toVal x)

data Val =
    Constr FieldName [Val]
  | Rec (M.Map FieldName Val)
  | VInt Int
  | VChar Char
  deriving (Eq, Show)

class ToVal a where
  toVal :: a -> Val
  default toVal :: (G.Generic a, All2 ToVal (GCode a), GFrom a, GDatatypeInfo a)
                => a -> Val
  toVal x = sopToVal (gdatatypeInfo (Proxy :: Proxy a)) (gfrom x)  

instance ToVal a => ToVal (Maybe a)

-- instance ToVal Val where
--     toVal = id

instance ToVal Int where toVal = VInt
instance ToVal Char where toVal = VChar
