{-# language DeriveGeneric #-}
module Analyze.Values (Value(..), ToValue(..), FromValue(..), valueToType, ValueType(..),
                      -- * Require functions
                      text, integer, int, double, bool,
                      -- ** Value type exceptions
                      ValueTypeError(..)) where

import           Analyze.Common      (Key)
import           Control.Monad.Catch (Exception, MonadThrow (..))
import           Data.Text           (Text)
import           Data.Typeable       (Typeable)
import qualified GHC.Generics as G
import Data.Hashable (Hashable(..))
import Prelude hiding (getChar)

-- | Interpret a 'Value' as a primitive type
class FromValue v where
  fromValue :: Value -> Maybe v

instance FromValue Int where fromValue = getInt
instance FromValue Integer where fromValue = getInteger
instance FromValue Double where fromValue = getDouble
instance FromValue Char where fromValue = getChar
instance FromValue Text where fromValue = getText
instance FromValue Bool where fromValue = getBool

-- | Wrap a primitive type into a 'Value'
class ToValue v where
  toValue :: v -> Value

instance ToValue Int where toValue = VInt
instance ToValue Integer where toValue = VInteger
instance ToValue Double where toValue = VDouble
instance ToValue Char where toValue = VChar
instance ToValue Text where toValue = VText
instance ToValue Bool where toValue = VBool

-- | Singleton type for value types.
data ValueType =
    VTypeText
  | VTypeChar
  | VTypeInteger
  | VTypeInt  
  | VTypeDouble
  | VTypeBool
  deriving (Show, Eq, Enum, Bounded)

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

-- | Returns the type of the value.
valueToType :: Value -> ValueType
valueToType (VText _)    = VTypeText
valueToType (VChar _)    = VTypeChar
valueToType (VInteger _) = VTypeInteger
valueToType (VInt _)     = VTypeInt
valueToType (VDouble _)  = VTypeDouble
valueToType (VBool _)    = VTypeBool

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

-- | Use with 'Analyze.Decoding.requireWhere' to read 'Text' values. Throws 'ValueTypeError' if the Value doesn't carry a value of the right type.
text :: (Key k, MonadThrow m) => k -> Value -> m Text
text _ (VText s) = pure s
text k v         = throwM (ValueTypeError k VTypeText v)

-- | Use with 'Analyze.Decoding.requireWhere' to read 'Integer' values. Throws 'ValueTypeError' if the Value doesn't carry a value of the right type.
integer :: (Key k, MonadThrow m) => k -> Value -> m Integer
integer _ (VInteger s) = pure s
integer k v            = throwM (ValueTypeError k VTypeInteger v)

-- | Use with 'Analyze.Decoding.requireWhere' to read 'Int' values. Throws 'ValueTypeError' if the Value doesn't carry a value of the right type.
int :: (Key k, MonadThrow m) => k -> Value -> m Int
int _ (VInt s) = pure s
int k v        = throwM (ValueTypeError k VTypeInt v)

-- | Use with 'Analyze.Decoding.requireWhere' to read 'Double' values. Throws 'ValueTypeError' if the Value doesn't carry a value of the right type.
double :: (Key k, MonadThrow m) => k -> Value -> m Double
double _ (VDouble s) = pure s
double k v           = throwM (ValueTypeError k VTypeDouble v)

-- | Use with 'Analyze.Decoding.requireWhere' to read 'Bool' values. Throws 'ValueTypeError' if the Value doesn't carry a value of the right type.
bool :: (Key k, MonadThrow m) => k -> Value -> m Bool
bool _ (VBool s) = pure s
bool k v         = throwM (ValueTypeError k VTypeBool v)
