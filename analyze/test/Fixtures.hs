module Fixtures where

import           Analyze.RFrame      (RFrame (..), RFrameUpdate (..))
import           Analyze.Values
import qualified Control.Foldl       as F
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

exampleDecl :: Vector (Text, ValueType)
exampleDecl = V.fromList
  [ ("id", ValueTypeInteger)
  , ("name", ValueTypeText)
  , ("score", ValueTypeDouble)
  ]

exampleHeader :: Vector Text
exampleHeader = V.fromList
  [ "id"
  , "name"
  , "score"
  ]

exampleObj1 :: Vector (Text, Value)
exampleObj1 = V.fromList
  [ ("id", ValueInteger 42)
  , ("name", ValueText "foo")
  , ("score", ValueDouble 5.0)
  ]

exampleRecord1 :: Vector Value
exampleRecord1 = V.fromList
  [ ValueInteger 42
  , ValueText "foo"
  , ValueDouble 50.0
  ]

exampleObj2 :: Vector (Text, Value)
exampleObj2 = V.fromList
  [ ("id", ValueInteger 43)
  , ("name", ValueText "bar")
  , ("score", ValueDouble 3.0)
  ]

fullUpdate :: RFrameUpdate Text Value
fullUpdate = RFrameUpdate names values
  where
    names = V.fromList ["id", "name", "score"]
    values = V.fromList
      [ V.fromList [ValueInteger 42, ValueText "foo", ValueDouble 5.0]
      , V.fromList [ValueInteger 43, ValueText "bar", ValueDouble 3.0]
      ]

noNameUpdate :: RFrameUpdate Text Value
noNameUpdate = RFrameUpdate names values
  where
    names = V.fromList ["id", "score"]
    values = V.fromList
      [ V.fromList [ValueInteger 42, ValueDouble 5.0]
      , V.fromList [ValueInteger 43, ValueDouble 3.0]
      ]

colorUpdate :: RFrameUpdate Text Value
colorUpdate = RFrameUpdate names values
  where
    names = V.fromList ["color"]
    values = V.fromList
      [ V.fromList [ValueText "purple"]
      , V.fromList [ValueText "orange"]
      ]

colorOneUpdate :: RFrameUpdate Text Value
colorOneUpdate = RFrameUpdate names values
  where
    names = V.fromList ["color"]
    values = V.fromList
      [ V.fromList [ValueText "purple"]
      ]

colorSpanishUpdate :: RFrameUpdate Text Value
colorSpanishUpdate = RFrameUpdate names values
  where
    names = V.fromList ["color", "spanishColor"]
    values = V.fromList
      [ V.fromList [ValueText "purple", ValueText "lila"]
      , V.fromList [ValueText "orange", ValueText "naranja"]
      ]

colorHotUpdate :: RFrameUpdate Text Value
colorHotUpdate = RFrameUpdate names values
  where
    names = V.fromList ["color/purple", "color/orange"]
    values = V.fromList
      [ V.fromList [ValueBool True, ValueBool False]
      , V.fromList [ValueBool False, ValueBool True]
      ]

fullColorUpdate :: RFrameUpdate Text Value
fullColorUpdate = RFrameUpdate names values
  where
    names = V.fromList ["id", "name", "score", "color"]
    values = V.fromList
      [ V.fromList [ValueInteger 42, ValueText "foo", ValueDouble 5.0, ValueText "purple"]
      , V.fromList [ValueInteger 43, ValueText "bar", ValueDouble 3.0, ValueText "orange"]
      ]

overlapUpdate :: RFrameUpdate Text Value
overlapUpdate = RFrameUpdate names values
  where
    names = V.fromList ["color", "score"]
    values = V.fromList
      [ V.fromList [ValueText "purple", ValueDouble 10.0]
      , V.fromList [ValueText "orange", ValueDouble 6.0]
      ]

fullOverlapUpdate :: RFrameUpdate Text Value
fullOverlapUpdate = RFrameUpdate names values
  where
    names = V.fromList ["id", "name", "score", "color"]
    values = V.fromList
      [ V.fromList [ValueInteger 42, ValueText "foo", ValueDouble 10.0, ValueText "purple"]
      , V.fromList [ValueInteger 43, ValueText "bar", ValueDouble 6.0, ValueText "orange"]
      ]

emptyUpdate :: RFrameUpdate Text Value
emptyUpdate = RFrameUpdate V.empty (V.replicate 2 V.empty)

fixtures :: HashMap Text (RFrameUpdate Text Value)
fixtures = HM.fromList
  [ ("full", fullUpdate)
  , ("noName", noNameUpdate)
  , ("color", colorUpdate)
  , ("colorOne", colorOneUpdate)
  , ("colorHot", colorHotUpdate)
  , ("colorSpanish", colorSpanishUpdate)
  , ("empty", emptyUpdate)
  , ("fullColor", fullColorUpdate)
  , ("overlap", overlapUpdate)
  , ("fullOverlap", fullOverlapUpdate)
  ]

exampleCsv :: Text
exampleCsv = "id,name,score\n" `mappend` "42,foo,5.0\n" `mappend` "43,bar,3.0\n"
