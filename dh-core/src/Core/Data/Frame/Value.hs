{-# language DeriveGeneric, DeriveDataTypeable #-}
module Core.Data.Frame.Value where

import qualified GHC.Generics as G
import Data.Text
import Data.Hashable
import Control.Exception (Exception(..))
import Data.Typeable (Typeable)
import Prelude hiding (getChar)

-- | Union type for values.
data Value =
    VText Text
  | VChar Char
  | VInteger Integer
  | VInt Int  
  | VDouble Double
  | VBool Bool
  deriving (Show, Eq, G.Generic)
instance Hashable Value

-- | Singleton type for value types.
data ValueType =
    VTypeText
  | VTypeChar
  | VTypeInteger
  | VTypeInt  
  | VTypeDouble
  | VTypeBool
  deriving (Show, Eq, Enum, Bounded)

-- | Returns the type of the value.
valueToType :: Value -> ValueType
valueToType (VText _)    = VTypeText
valueToType (VChar _)    = VTypeChar
valueToType (VInteger _) = VTypeInteger
valueToType (VInt _)     = VTypeInt
valueToType (VDouble _)  = VTypeDouble
valueToType (VBool _)    = VTypeBool

-- | Interpret a 'Value' as a primitive type
class FromValue v where
  fromValue :: Value -> Maybe v

instance FromValue Int where fromValue = getInt
instance FromValue Integer where fromValue = getInteger
instance FromValue Double where fromValue = getDouble
instance FromValue Char where fromValue = getChar
instance FromValue Text where fromValue = getText
instance FromValue Bool where fromValue = getBool

-- | Wrap a primitive type into a 'Maybe Value'
-- 
-- This encodes the possibility of missing features
class ToValueM v where
  toValueM :: v -> Maybe Value


instance ToValueM Int where toValueM = pure . VInt
instance ToValueM Integer where toValueM = pure . VInteger
instance ToValueM Double where toValueM = pure . VDouble
instance ToValueM Char where toValueM = pure . VChar
instance ToValueM Text where toValueM = pure . VText
instance ToValueM Bool where toValueM = pure . VBool

instance ToValue a => ToValueM (Maybe a) where
  toValueM Nothing = Nothing
  toValueM (Just x) = Just $ toValue x

instance (ToValue l, ToValue r) => ToValueM (Either l r) where
  toValueM (Left x) = Just $ toValue x
  toValueM (Right y) = Just $ toValue y



-- | Wrap a primitive type into a 'Value'
class ToValue v where
  toValue :: v -> Value

instance ToValue Int where toValue = VInt
instance ToValue Integer where toValue = VInteger
instance ToValue Double where toValue = VDouble
instance ToValue Char where toValue = VChar
instance ToValue Text where toValue = VText
instance ToValue Bool where toValue = VBool


-- | Extracts 'Text' from the 'Value'.
getText :: Value -> Maybe Text
getText (VText s) = Just s
getText _         = Nothing

-- | Extracts 'Char' from the 'Value'.
getChar :: Value -> Maybe Char
getChar (VChar s) = Just s
getChar _         = Nothing 

-- | Extracts 'Integer' from the 'Value'.
getInteger :: Value -> Maybe Integer
getInteger (VInteger i) = Just i
getInteger _            = Nothing

-- | Extracts 'Int' from the 'Value'.
getInt :: Value -> Maybe Int
getInt (VInt i) = Just i
getInt _        = Nothing

-- | Extracts 'Double' from the 'Value'.
getDouble :: Value -> Maybe Double
getDouble (VDouble d) = Just d
getDouble _           = Nothing

-- | Extracts 'Bool' from the 'Value'.
getBool :: Value -> Maybe Bool
getBool (VBool b) = Just b
getBool _         = Nothing


-- | Exception for when we encounder unexpected values.
data ValueTypeError k = ValueTypeError k ValueType Value deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (ValueTypeError k)
