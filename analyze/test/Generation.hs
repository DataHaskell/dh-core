module Generation where

import           Analyze.Common  (Key, makeLookup)
import           Analyze.Frame.Dense (RFrame (..))
import           Analyze.Values
import           Data.HashSet    (HashSet)
import qualified Data.HashSet    as HS
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Vector     (Vector)
import qualified Data.Vector     as V
import           Test.QuickCheck

distinctGenSized :: Key k => Gen k -> Int -> Gen (HashSet k)
distinctGenSized = go HS.empty
  where
    go s g i | i <= 0 = pure s
             | otherwise = do
                k <- g `suchThat` \k' -> not (HS.member k' s)
                go (HS.insert k s) g (i - 1)

distinctGen :: Key k => Gen k -> Gen (HashSet k)
distinctGen = sized . distinctGenSized

declGenSized :: Key k => Gen k -> Gen t -> Int -> Gen (Vector (k, t))
declGenSized kg tg i = do
  nameSet <- distinctGen kg
  let nameVec = V.fromList (HS.toList nameSet)
  valueTypeVec <- V.replicateM i tg
  pure (V.zip nameVec valueTypeVec)

declGen :: Key k => Gen k -> Gen t -> Gen (Vector (k, t))
declGen kg tg = sized (declGenSized kg tg)

rframeGenSized :: Key k => (t -> Gen v) -> Vector (k, t) -> Int -> Gen (RFrame k v)
rframeGenSized prod decl numRows = gen
  where
    rowGen = sequenceA (prod . snd <$> decl)
    allRowsGen = V.replicateM numRows rowGen
    keys = fst <$> decl
    gen = RFrame keys (makeLookup keys) <$> allRowsGen

rframeGen :: Key k => (t -> Gen v) -> Vector (k, t) -> Gen (RFrame k v)
rframeGen prod decl = sized (rframeGenSized prod decl)

-- Specifics

nameGen :: Gen Text
nameGen = T.pack <$> listOf (choose ('a', 'z'))

valueGen :: ValueType -> Gen Value
valueGen VTypeText    = VText <$> nameGen
valueGen VTypeInteger = VInteger <$> arbitrary
valueGen VTypeDouble  = VDouble <$> arbitrary

valueTypeGen :: Gen ValueType
valueTypeGen = arbitraryBoundedEnum

valueDeclGen :: Gen (Vector (Text, ValueType))
valueDeclGen = declGen nameGen valueTypeGen

valueRFrameGen :: Gen (RFrame Text Value)
valueRFrameGen = valueDeclGen >>= rframeGen valueGen
