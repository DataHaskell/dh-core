-- | Simple value types and functions.
module Analyze.Values where

import           Analyze.Common      (Data)
import           Control.Monad.Catch (Exception, MonadThrow (..))
import           Data.Text           (Text)
import           Data.Typeable       (Typeable)

-- | Singleton type for value types.
data ValueType =
    ValueTypeText
  | ValueTypeInteger
  | ValueTypeDouble
  | ValueTypeBool
  deriving (Show, Eq, Enum, Bounded)

-- | Union type for values.
data Value =
    ValueText Text
  | ValueInteger Integer
  | ValueDouble Double
  | ValueBool Bool
  deriving (Show, Eq)

-- | Returns the type of the value.
valueToType :: Value -> ValueType
valueToType (ValueText _)    = ValueTypeText
valueToType (ValueInteger _) = ValueTypeInteger
valueToType (ValueDouble _)  = ValueTypeDouble
valueToType (ValueBool _)    = ValueTypeBool

-- | Extracts 'Text' from the 'Value'.
getText :: Value -> Maybe Text
getText (ValueText s) = Just s
getText _             = Nothing

-- | Extracts 'Integer' from the 'Value'.
getInteger :: Value -> Maybe Integer
getInteger (ValueInteger i) = Just i
getInteger _                = Nothing

-- | Extracts 'Double' from the 'Value'.
getDouble :: Value -> Maybe Double
getDouble (ValueDouble d) = Just d
getDouble _               = Nothing

-- | Extracts 'Bool' from the 'Value'.
getBool :: Value -> Maybe Bool
getBool (ValueBool b) = Just b
getBool _             = Nothing

-- | Exception for when we encounder unexpected values.
data ValueTypeError k = ValueTypeError k ValueType Value deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (ValueTypeError k)

-- | Use with 'Analyze.Decoding.requireWhere' to read 'Text' values.
textual :: (Data k, MonadThrow m) => k -> Value -> m Text
textual _ (ValueText s) = pure s
textual k v             = throwM (ValueTypeError k ValueTypeText v)

-- | Use with 'Analyze.Decoding.requireWhere' to read 'Integer' values.
integral :: (Data k, MonadThrow m) => k -> Value -> m Integer
integral _ (ValueInteger s) = pure s
integral k v                = throwM (ValueTypeError k ValueTypeInteger v)

-- | Use with 'Analyze.Decoding.requireWhere' to read 'Double' values.
floating :: (Data k, MonadThrow m) => k -> Value -> m Double
floating _ (ValueDouble s) = pure s
floating k v               = throwM (ValueTypeError k ValueTypeDouble v)

-- | Use with 'Analyze.Decoding.requireWhere' to read 'Bool' values.
boolean :: (Data k, MonadThrow m) => k -> Value -> m Bool
boolean _ (ValueBool s) = pure s
boolean k v             = throwM (ValueTypeError k ValueTypeBool v)
