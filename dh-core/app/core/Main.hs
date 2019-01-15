{-# language DeriveDataTypeable, DeriveGeneric #-}
{-# language OverloadedStrings #-}
module Main where

-- import qualified Analyze.RFrame as AR
import qualified Analyze.Values as AV
-- import qualified Analyze.Values.Generic as AVG
-- import qualified Analyze.RFrame.Generic as ARG
import Analyze.Dplyr 
import Analyze.Dplyr.Generic (gToTable)

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


data Price = Price { item :: T.Text, price :: Double } deriving (Eq, Show, G.Generic, Data)
instance Generic Price
data Purchase = Purchase { date :: Int, person :: T.Text, itemBought :: T.Text, qtyBought :: Int } deriving (Eq, Show, G.Generic, Data)
instance Generic Purchase

prices :: [Price]
prices = [
    Price "computer" 1000
  , Price "car" 5000
  , Price "legal fees" 400
         ]

purchases :: [Purchase]         
purchases = [
    Purchase 7 "bob" "car" 1
  , Purchase 7 "alice" "car" 1
  , Purchase 4 "bob" "legal fees" 20
  , Purchase 3 "alice" "computer" 2
  , Purchase 1 "bob" "computer" 1
            ]         





-- prog tab0 = do
--   tab1 <- filterByElem ff k tab0
--   tabJ <- innerJoin tab1 pr




  



main = putStrLn "hello!"






