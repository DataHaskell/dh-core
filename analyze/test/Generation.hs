module Generation where

import           Analyze.Common  (Data, makeLookup)
import           Analyze.RFrame  (RFrame (..),RFrameUpdate (..))
import           Analyze.Values
import           Data.HashSet    (HashSet)
import qualified Data.HashSet    as HS
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Vector     (Vector)
import qualified Data.Vector     as V
import           Test.QuickCheck

distinctGenSized :: Data k => Gen k -> Int -> Gen (HashSet k)
distinctGenSized = go HS.empty
  where
    go s g i | i <= 0 = pure s
             | otherwise = do
                k <- g `suchThat` \k' -> not (HS.member k' s)
                go (HS.insert k s) g (i - 1)

distinctGen :: Data k => Gen k -> Gen (HashSet k)
distinctGen = sized . distinctGenSized

declGenSized :: Data k => Gen k -> Gen t -> Int -> Gen (Vector (k, t))
declGenSized kg tg i = do
  nameSet <- distinctGen kg
  let nameVec = V.fromList (HS.toList nameSet)
  valueTypeVec <- V.replicateM i tg
  pure (V.zip nameVec valueTypeVec)

declGen :: Data k => Gen k -> Gen t -> Gen (Vector (k, t))
declGen kg tg = sized (declGenSized kg tg)

rframeGenSized :: Data k => (t -> Gen v) -> Vector (k, t) -> Int -> Gen (RFrame k v)
rframeGenSized prod decl numRows = gen
  where
    rowGen = sequenceA (prod . snd <$> decl)
    allRowsGen = V.replicateM numRows rowGen
    keys = fst <$> decl
    gen = RFrame keys (makeLookup keys) <$> allRowsGen

rframeGen :: Data k => (t -> Gen v) -> Vector (k, t) -> Gen (RFrame k v)
rframeGen prod decl = sized (rframeGenSized prod decl)

-- needed to generate an updated, copied off the 
rframeUpdateGenSized :: Data k => (t -> Gen v) -> Vector (k, t) -> Int -> Gen (RFrameUpdate k v)
rframeUpdateGenSized prod decl numRows = gen
  where
    rowGen = sequenceA (prod . snd <$> decl)
    allRowsGen = V.replicateM numRows rowGen
    keys = fst <$> decl
    gen = RFrameUpdate keys <$> allRowsGen

-- again some machinery 
rframeUpdateGen :: Data k => (t -> Gen v) -> Vector (k, t) -> Gen (RFrameUpdate k v)
rframeUpdateGen prod decl = sized (rframeUpdateGenSized prod decl)

-- Specifics

nameGen :: Gen Text
nameGen = T.pack <$> listOf (choose ('a', 'z'))

valueGen :: ValueType -> Gen Value
valueGen ValueTypeText    = ValueText <$> nameGen
valueGen ValueTypeInteger = ValueInteger <$> arbitrary
valueGen ValueTypeDouble  = ValueDouble <$> arbitrary
valueGen ValueTypeBool    = ValueBool <$> arbitrary

valueTypeGen :: Gen ValueType
valueTypeGen = arbitraryBoundedEnum

valueDeclGen :: Gen (Vector (Text, ValueType))
valueDeclGen = declGen nameGen valueTypeGen

valueRFrameGen :: Gen (RFrame Text Value)
valueRFrameGen = valueDeclGen >>= rframeGen valueGen


-- generates an update
valueRFrameUpdateGen :: Gen (RFrameUpdate Text Value)
valueRFrameUpdateGen = valueDeclGen >>= rframeUpdateGen valueGen


-- things down there serve to produce updates with only doubles
-- only ouputs Vector (Text, ValueTypeDouble))
valueDeclGenDouble :: Gen (Vector (Text, ValueType))
valueDeclGenDouble = declGen nameGen (elements [ValueTypeDouble])

-- the actual generator
doubleRFrameUpdateGen :: Gen (RFrameUpdate Text Value)
doubleRFrameUpdateGen = valueDeclGenDouble >>= rframeUpdateGen valueGen-- a frame generator that will only have Double's as data