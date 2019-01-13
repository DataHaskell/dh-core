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
  [ ("id", VTypeInteger)
  , ("name", VTypeText)
  , ("score", VTypeDouble)
  ]

exampleHeader :: Vector Text
exampleHeader = V.fromList
  [ "id"
  , "name"
  , "score"
  ]

exampleObj1 :: Vector (Text, Value)
exampleObj1 = V.fromList
  [ ("id", VInteger 42)
  , ("name", VText "foo")
  , ("score", VDouble 5.0)
  ]

exampleRecord1 :: Vector Value
exampleRecord1 = V.fromList
  [ VInteger 42
  , VText "foo"
  , VDouble 50.0
  ]

exampleObj2 :: Vector (Text, Value)
exampleObj2 = V.fromList
  [ ("id", VInteger 43)
  , ("name", VText "bar")
  , ("score", VDouble 3.0)
  ]

fullUpdate :: RFrameUpdate Text Value
fullUpdate = RFrameUpdate names values
  where
    names = V.fromList ["id", "name", "score"]
    values = V.fromList
      [ V.fromList [VInteger 42, VText "foo", VDouble 5.0]
      , V.fromList [VInteger 43, VText "bar", VDouble 3.0]
      ]

noNameUpdate :: RFrameUpdate Text Value
noNameUpdate = RFrameUpdate names values
  where
    names = V.fromList ["id", "score"]
    values = V.fromList
      [ V.fromList [VInteger 42, VDouble 5.0]
      , V.fromList [VInteger 43, VDouble 3.0]
      ]

colorUpdate :: RFrameUpdate Text Value
colorUpdate = RFrameUpdate names values
  where
    names = V.fromList ["color"]
    values = V.fromList
      [ V.fromList [VText "purple"]
      , V.fromList [VText "orange"]
      ]

colorOneUpdate :: RFrameUpdate Text Value
colorOneUpdate = RFrameUpdate names values
  where
    names = V.fromList ["color"]
    values = V.fromList
      [ V.fromList [VText "purple"]
      ]

colorSpanishUpdate :: RFrameUpdate Text Value
colorSpanishUpdate = RFrameUpdate names values
  where
    names = V.fromList ["color", "spanishColor"]
    values = V.fromList
      [ V.fromList [VText "purple", VText "lila"]
      , V.fromList [VText "orange", VText "naranja"]
      ]

colorHotUpdate :: RFrameUpdate Text Value
colorHotUpdate = RFrameUpdate names values
  where
    names = V.fromList ["color/purple", "color/orange"]
    values = V.fromList
      [ V.fromList [VBool True, VBool False]
      , V.fromList [VBool False, VBool True]
      ]

fullColorUpdate :: RFrameUpdate Text Value
fullColorUpdate = RFrameUpdate names values
  where
    names = V.fromList ["id", "name", "score", "color"]
    values = V.fromList
      [ V.fromList [VInteger 42, VText "foo", VDouble 5.0, VText "purple"]
      , V.fromList [VInteger 43, VText "bar", VDouble 3.0, VText "orange"]
      ]

overlapUpdate :: RFrameUpdate Text Value
overlapUpdate = RFrameUpdate names values
  where
    names = V.fromList ["color", "score"]
    values = V.fromList
      [ V.fromList [VText "purple", VDouble 10.0]
      , V.fromList [VText "orange", VDouble 6.0]
      ]

fullOverlapUpdate :: RFrameUpdate Text Value
fullOverlapUpdate = RFrameUpdate names values
  where
    names = V.fromList ["id", "name", "score", "color"]
    values = V.fromList
      [ V.fromList [VInteger 42, VText "foo", VDouble 10.0, VText "purple"]
      , V.fromList [VInteger 43, VText "bar", VDouble 6.0, VText "orange"]
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
