{-# LANGUAGE OverloadedStrings, GADTs #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- |
Module      :  Numeric.Datasets.Internal.ArffParser
Description :  Parser for datasets in the Atrribute-Relation File Format (ARFF)
Copyright   :  (c) Arvind Devarajan
License     :  BSD-3-Clause

Maintainer  :  arvindd
Stability   :  experimental
Portability :  portable
-}

module Numeric.Datasets.Internal.ArffParser
       ( parseArff
       , arffRecords
       , value
       , dblval
       , strval
       , dtval
       , Attribute
       , ArffRecord
       ) where

import Prelude hiding (take, takeWhile)        
import Control.Applicative ((<$>), (<|>)) 
import Data.Dynamic
import Data.Maybe
import qualified Data.ByteString as B (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as B8 (pack, unpack)
import qualified Data.ByteString.Lazy as BL (ByteString, toStrict)
import Data.Word8 (Word8, toLower)
import Data.Attoparsec.ByteString.Lazy hiding (satisfy)
import Data.Attoparsec.ByteString.Char8
          hiding (skipWhile, takeWhile, inClass, notInClass, eitherResult, parse)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Format (parseTimeM, iso8601DateFormat, defaultTimeLocale)
import Control.Exception (Exception, TypeError, throw)
import Data.Vector (fromList)

-- | Data types of attributes 
data DataType = Numeric 
              | Integer
              | Real
              | Class 
              | String
              | Date 
              | Relational
        deriving (Show)
 
{-|
  Attributes can be of two forms:
  - Name of the attribute, along with its data type
  - Class (or nominal form), with valid values for the class
-}
data Attribute where
    -- | Attr <name> <datatype>   
    Attr :: {
      attname :: B.ByteString -- ^ Name of the attribute
    , atttype :: DataType   -- ^ DataType of the attribute
    } -> Attribute

    -- | AttCls <class name> <class-element names>
    AttCls :: {
      attname :: B.ByteString -- ^ Name of the attribute class
    , attclasses :: [B.ByteString] 
    -- ^ The names of the elements of this class
    } -> Attribute 
    deriving (Show)

{- |
  Exception thrown when a reserved / future keyword from
  the ARFF file was used in the file that is parsed
-}
data NotImplementedException = 
  RelationalAttributeException 
  -- ^ Relational attribute was parsed, and this is reserved for the future
  | ReservedException
  -- ^ Some reserved keyword was parsed

  | UnknownAttributeTypeException
  -- ^ Attribute type error - could not parse / invalid attribute type
  deriving (Show)
instance Exception NotImplementedException

{- |
 Exception thrown when a data record's field value is not of the
 type as defined by the attributes in the ARFF file.
-}
data InvalidRecordException =
  InvalidFieldTypeException
  deriving (Show)
instance Exception InvalidRecordException

-- | Type for each data record in the ARFF file
type ArffRecord = [Maybe Dynamic]

-------------------- Parsing functions ----------------------

-- | Parse the ARFF file, and return (Relation name, Attributes, ARFF Records)
-- | If you just want the ARFF data records, use the arffRecords function
parseArff :: Parser (B.ByteString, [Attribute], [ArffRecord])
parseArff = do  
  spaces >> skipMany comment >> spaces
  rel <- relation
  spaces >> skipMany comment
  atts <- many' attribute
  spaces >> skipMany comment
  stringCI "@data" >> spaces
  spaces >> skipMany comment
  dat <- manyTill (record atts) endOfInput   
  spaces >> skipMany comment
  return (rel, atts, dat)

-- | Decode and extract only the records returned by the ARFF parser
-- | If all metadata is also required (i.e., relation name, and attributes,
-- | use the parseArff above)
arffRecords :: Parser [ArffRecord]
arffRecords = do
  (rel, atts, dats) <- parseArff
  return dats

---------------- Convenience functions to get field values ---------

{- | 
  Retrieves double-values from the ARFF record, supplying 0 for missing values
-}
dblval :: Int -> ArffRecord -> Double
dblval idx r = value (\_->0) idx r

{- | 
  Retrieves string-values from the ARFF record, supplying "" for missing values
-}
strval :: Int -> ArffRecord -> BL.ByteString
strval idx r = value (\_->"") idx r

{- | 
  Retrieves date-values from the ARFF record, supplying the default
  gregorian date for '0' for missing values. According to the
  official documentation, this date corresponds to: 1858-11-17.
-}
dtval :: Int -> ArffRecord -> Day
dtval idx r = value (\_->fromGregorian 0 0 0) idx r

{- | 
  Get value of any type field from the ARFF record

  This is function allows for the user to also implement a dataset-specific strategy for supplying the missing values. The first parameter of this function is a function that implements this missing-value-strategy.
-}
value :: Typeable a
      => (ArffRecord -> a)  -- ^ Function implementing missing-value-strategy
                            -- The output of this function will be used when
                            -- the parser encounters a missing value in the
                            -- dataset.
      -> Int                -- ^ Index of the field whose value is needed
      -> ArffRecord         -- ^ ARFF record
      -> a                  -- Value of the field
value f idx r = let missingval = f r
                    maybeval = r !! idx
                    dynval = fromMaybe (toDyn missingval) maybeval
                    defval = missingval
                in fromDyn dynval defval

----------------------- All parsers --------------------------
spaces :: Parser ()
spaces = skipWhile (\x -> isSpace_w8 x || isEndOfLine x)

eol :: Parser ()
eol = endOfLine <|> endOfInput <|> comment

comment :: Parser ()
comment = char '%' >> 
  skipWhile (\x->not $ isEndOfLine x) >> 
  (endOfLine <|> endOfInput)

-- | Parses names (of classes, attributes, etc.) 
name :: Parser B.ByteString
name = do
  spaces
  n <- (quotedName <|> unquotedName)
  return n

-- | Name of a relation or an attribute
quotedName :: Parser B.ByteString
quotedName = do
    quote <- char '"' <|> char '\''

    -- ARFF format says make sure that the quote is not preceded by
    -- a '\' character. Presence of such a character "escapes" the
    -- quote - this allows for a string to contain a ' by just
    -- preceding it with a quote.
    n <- B.pack <$> manyTill anyWord8 (notChar '\\' >> char quote)
    return n

unquotedName :: Parser B.ByteString
-- We use isHorizontalSpace instead of isSpace_w8 below because
-- we want to only capture the character " " or "\t" and not newlines
unquotedName = takeWhile (\x -> (notInClass ",}\'\"" x) 
   && not (isHorizontalSpace x) && not (isEndOfLine x))

relation :: Parser B.ByteString
relation = stringCI "@relation" >> spaces >> name

{- |
Consumes attributes: depending on how these are defined in the
ARFF file, these can be attributes with some data-type or 
attribute classes. Returns one of these:

Attr <attr-name> <attr-type>

OR

AttCls <attclass-name> <att-class-element-names>
-}
attribute :: Parser Attribute
attribute = do
    stringCI "@attribute" >> spaces 
    n <- name
    spaces
    c <- peekChar'
    case c of
        '{' -> createAttCls n
        _   -> createAttType n
  where
    createAttCls :: B.ByteString -> Parser Attribute
    -- ^ Create an attribute with AttCls, given the attribute's name
    createAttCls n = do
        char '{' >> spaces  
        vals <- name `sepBy` (char ',')
        char '}' >> spaces
        return $ AttCls n vals

    createAttType :: B.ByteString -> Parser Attribute
    -- ^ Create an attribute with Attr, given the attribute's name
    createAttType n = do
        t <- spaces >> manyTill anyWord8 eol
        return $ Attr n (dattype t)
      where
        dattype :: [Word8] -> DataType            
        dattype s = case (strToLower s) of
            "numeric"    -> Numeric            
            "integer"    -> Integer
            "real"       -> Real
            "string"     -> String
            "date"       -> Date
            "relational" -> Relational
            _            -> throw UnknownAttributeTypeException
          where
            strToLower :: [Word8] -> B.ByteString
            strToLower sz = B.pack [toLower x | x <- sz]

----------------------- Parsers for parsing CSV data records ----------

{- |
  Return a data record, and consume any comments following
  the record. This function takes care of situations where
  the ARFF files end with empty comment lines. 
-}
record :: [Attribute] -> Parser [Maybe Dynamic]
record atts = do
  vals <- datavals atts
  skipMany comment >> spaces
  return vals

{- | 
 Return data values as a list of Maybe Dynamics. Each data-value
 is a "Just <value>" (if there is a value) or Nothing (if the
 value is missing - i.e., has a '?' in the data record). 
-}
datavals :: [Attribute] -> Parser [Maybe Dynamic]
datavals [] = return []
datavals (a:as) = (:) <$> fieldval a <*> datavals as

{- |
 Reads a field from the data record, and returns its value as 
 a (Just Dynamic). If the value of the field is not of an 
 expected type (as defined by the attributes in the file), throws
 an InvalidFieldTypeException.

 As per ARFF, a field value can be missing (indicated as '?').
 In case the field is missing, its value is retuned as Nothing. 
-}
fieldval :: Attribute -> Parser (Maybe Dynamic)
-- ^ When attribute has a datatype
fieldval (Attr _ t) = do
  c <- peekChar'
  if (c == '?') then missing
  else dynVal   
    where dynVal :: Parser (Maybe Dynamic)
          dynVal = case t of
            Numeric    -> (Just . toDyn) <$> doublefield 
            Integer    -> (Just . toDyn) <$> doublefield
            Real       -> (Just . toDyn) <$> doublefield
            String     -> (Just . toDyn) <$> stringfield
            Date       -> (Just . toDyn) <$> datefield
            Relational -> throw RelationalAttributeException
            _          -> throw UnknownAttributeTypeException

fieldval (AttCls _ cls) = do
-- ^ When attribute is a nominal attribute with class-names
  c <- peekChar'
  if (c == '?') then missing
  else dynVal   
    where dynVal :: Parser (Maybe Dynamic)
          dynVal = do
            val <- stringfield
            if (val `elem` cls) then
              return $ Just (toDyn val)
            else
              throw InvalidFieldTypeException

stringfield :: Parser B.ByteString
stringfield = field name

doublefield :: Parser Double
doublefield = field double

missing :: Parser (Maybe Dynamic)
missing = do
  _ <- field (char '?') -- Consume the missing data value '?'
  return Nothing

datefield :: Parser Day
datefield = field date
  where
    date :: Parser Day
    date = do
      d <- word 
      f <- option defformat word
      parseTimeM False defaultTimeLocale (B8.unpack f) (B8.unpack d)
    
    defformat :: B.ByteString
    defformat = B8.pack $ iso8601DateFormat Nothing

------------- CSV Helper functions -------------------------

-- Read a word (delimited by a fieldseparator)
word :: Parser B.ByteString
word = takeWhile (\x->(not $ isSpace_w8 x) && (notInClass "," x))

-- Consume the field separator
fieldSeparator :: Parser B.ByteString
fieldSeparator = takeWhile (\x->(isSpace_w8 x) || (inClass "," x))

-- Return the value parsed by p, after consuming the field separator
field :: (Show a) => Parser a -> Parser a
field p = do
  val <- p
  _ <- fieldSeparator -- Consume the field separator
  return val