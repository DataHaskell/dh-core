{-# language DeriveDataTypeable, DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Applicative (Alternative(..))

-- import qualified Analyze.RFrame as AR
import Analyze.Common (Key(..))
import qualified Analyze.Values as AV
import qualified Analyze.Decoding as AD
-- import qualified Analyze.Values.Generic as AVG
-- import qualified Analyze.RFrame.Generic as ARG
import Analyze.Dplyr 
import Analyze.Dplyr.Generic (gToTable, gToRow)

import qualified GHC.Generics as G (Generic(..))

-- | generics-sop
import Generics.SOP (Generic(..))

-- | typeable
import Data.Data (Typeable, Data(..))

import Data.Hashable (Hashable(..))
-- import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow(..))

import qualified Data.Text as T
-- import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM

import Prelude hiding (filter, scanl, scanr, lookup)


data Price = Price { item :: T.Text, price :: Double } deriving (Eq, Show, G.Generic, Data)
instance Generic Price
data Purchase = Purchase { date :: Int, person :: T.Text, itemBought :: T.Text, qtyBought :: Int } deriving (Eq, Show, G.Generic, Data)
instance Generic Purchase

-- https://github.com/DataHaskell/dh-core/issues/17

prices_ :: [Price]
prices_ = [
    Price "computer" 1000
  , Price "car" 5000
  , Price "legal fees" 400
         ]

purchases_ :: [Purchase]         
purchases_ = [
    Purchase 7 "bob" "car" 1
  , Purchase 5 "alice" "car" 1
  , Purchase 4 "bob" "legal fees" 20
  , Purchase 3 "alice" "computer" 2
  , Purchase 1 "bob" "computer" 1
            ]



pricesTable, purchasesTable :: MonadThrow m => m (Table (Row T.Text AV.Value)) 
pricesTable = gToTable prices_
purchasesTable = gToTable purchases_



prog = do
  prices <- pricesTable
  purchases <- purchasesTable
  p1 <- filterByKey (/= AV.VText "legal fees") "itemBought" purchases
  innerJoin "item" "itemBought" prices p1






data Moo = Moo { m1 :: Double, m2 :: Double, m3 :: Double} deriving (Eq, Show, G.Generic, Data)
instance Generic Moo

-- withDoubles :: (Key k, MonadThrow m) =>
--                (Double -> Double -> b) -> k -> k -> AD.Decoder m k AV.Value b
withDoubles f k1 k2 =
  f <$> withDouble k1 <*> withInt k2 

decodeRow :: Key k => AD.Decoder Maybe k v a -> Row k v -> Maybe a
decodeRow dec row = AD.runDecoder dec (`lookup` row)

withDouble :: (Key k, MonadThrow m) => k -> AD.Decoder m k AV.Value Double
withDouble k = AD.requireWhere k AV.double

withInt k = fromIntegral <$> AD.requireWhere k AV.int




withNum2 :: (Key k, MonadThrow m) =>
            (Double -> Double -> b) -> k -> k -> AD.DecAlt m k AV.Value b
withNum2 f k1 k2 = f <$> withNumAlt k1 <*> withNumAlt k2

withNumAlt :: (Key k, MonadThrow m) => k -> AD.DecAlt m k AV.Value Double
withNumAlt k = fromIntegral <$> AD.requireWhereA k AV.int <|> AD.requireWhereA k AV.double



m00 = Moo 0.3 0.5 10.2

progMoo = do
  m <- gToRow ["m1", "m2", "m3"] m00
  let dec = withDoubles (+) "m1" "m3"
  -- print $ AD.decoderKeys dec
  decodeRow dec m




main = putStrLn "hello!"






