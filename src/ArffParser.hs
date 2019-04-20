{-# LANGUAGE OverloadedStrings, GADTs #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- |
Module      :  ArffParser
Description :  Parser for datasets in the Atrribute-Relation File Format (ARFF)
Copyright   :  (c) Arvind Devarajan
License     :  MIT

Maintainer  :  arvindd
Stability   :  unstable
Portability :  portable
-}

module ArffParser where
    -- ( parseArff
    -- , DataType
    -- , Attribute
    -- , Content
    -- ) where

import Prelude hiding (take, takeWhile)        
import Control.Applicative ((<$>), (<|>)) 
import Data.Dynamic
import Data.ByteString (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as BC8 (pack, unpack)
import Data.Word8 (Word8, toLower)
import Data.Attoparsec.ByteString.Lazy hiding (satisfy)
import Data.Attoparsec.ByteString.Char8
          hiding (skipWhile, takeWhile, takeWhile1, inClass, notInClass)
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM, iso8601DateFormat, defaultTimeLocale)
import Control.Exception (Exception, TypeError, throw)

-- | Data types of attributes 
data DataType = Numeric 
              | Integer
              | Real
              | Class 
              | String
              | Date 
              | Relational
              | Unknown
        deriving (Show)
 
{-|
  Attributes can be of two forms:
  - Name of the attribute, along with its data type
  - Class (or nominal form), with valid values for the class
-}
data Attribute where
    -- | Attr <name> <datatype>   
    Attr :: {
      attname :: ByteString -- ^ Name of the attribute
    , atttype :: DataType   -- ^ DataType of the attribute
    } -> Attribute

    -- | AttCls <class names>
    AttCls :: {
      attclasses :: [ByteString] 
      -- ^ The names of the classes of the "class-type" attribute
    } -> Attribute 
    deriving (Show)

-- | Type for each data record in the ARFF file
type ArffRecord = [Dynamic]

-- | Parse the ARFF file, and return (Relation name, ARFF Records)
parseArff :: Parser ([Attribute], [ArffRecord])
parseArff = do  
    skipMany comment >> spaces
    rel <- relation 
    skipMany comment >> spaces
    atts <- many' attribute
    skipMany comment >> spaces
    stringCI "@data" >> spaces
    skipMany comment >> spaces
    dat <- manyTill (record atts) endOfInput   
    return (atts, dat)

----------------------- All parsers --------------------------
spaces :: Parser ()
spaces = skipWhile (\x -> isSpace_w8 x || isEndOfLine x)

comment :: Parser ()
comment = char '%' >> manyTill anyWord8 endOfLine >> return ()

eol :: Parser ()
eol = endOfLine <|> comment

-- | Name of a relation or an attribute
quotedName :: Parser ByteString
quotedName = do
    quote <- char '"' <|> char '\''
    pack <$> manyTill anyWord8 (char quote)

unquotedName :: Parser ByteString
-- We use isHorizontalSpace instead of isSpace_w8 below because
-- we want to only capture the character " " or "\t" and not newlines
unquotedName = takeWhile (\x -> not (isHorizontalSpace x))

relation :: Parser ByteString
relation = stringCI "@relation" >> spaces >> pack <$> manyTill anyWord8 eol

attribute :: Parser Attribute
attribute = do
    stringCI "@attribute" >> spaces 
    c <- stringCI "class" <|> unquotedName <|> quotedName
    case c of
        "class"     -> attclass
        _           -> atttype c
  where
    attclass :: Parser Attribute
    attclass = do
        spaces >> char '{'
        vals <- clsname `sepBy` (char ',')
        char '}'
        return $ AttCls vals
      where
        clsname :: Parser ByteString
        clsname = takeWhile1 (\x -> notInClass ",}" x)   

    atttype :: ByteString -> Parser Attribute
    atttype c = do
        t <- spaces >> manyTill anyWord8 eol
        return $ Attr c (dattype t)
      where
        dattype :: [Word8] -> DataType            
        dattype s = case (strToLower s) of
            "numeric"    -> Numeric            
            "integer"    -> Integer
            "real"       -> Real
            "string"     -> String
            "date"       -> Date
            "relational" -> Relational
            _            -> Unknown
          where
            strToLower :: [Word8] -> ByteString
            strToLower s = pack [toLower x | x <- s]

----------------------- Parsers for parsing CSV data records ----------

{- |
  Exception thrown when a reserved / future keyword from
  the ARFF file was used in the file that is parsed
-}
data NotImplementedException = 
  RelationalAttributeException | ReservedException
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

-- | Return a data record as a list of Dynamics  
record :: [Attribute] -> Parser [Dynamic]
record [] = return []
record (a:as) = (:) <$> fieldval a <*> record as

{- |
 Reads a field from the data record, and returns its value as
 a Dynamic. If the value of the field is not of an expected
 type (as defined by the attributes in the file), throws
 an InvalidFieldTypeException.
-}
fieldval :: Attribute -> Parser Dynamic
fieldval (Attr _ t) = case t of
  Numeric    -> toDyn <$> doublefield 
  Integer    -> toDyn <$> doublefield
  Real       -> toDyn <$> doublefield
  String     -> toDyn <$> stringfield
  Date       -> toDyn <$> datefield
  Relational -> throw RelationalAttributeException
fieldval (AttCls cls) = do
  val <- stringfield
  if (val `elem` cls) then
    return $ toDyn val
  else
    throw InvalidFieldTypeException

stringfield :: Parser ByteString
stringfield = field word

doublefield :: Parser Double
doublefield = field double

datefield :: Parser (Maybe Day)
datefield = field date
  where
    date :: Parser (Maybe Day)
    date = do
      d <- word 
      f <- option defformat word
      return $ parseTimeM False defaultTimeLocale (BC8.unpack f) (BC8.unpack d)
    
    defformat :: ByteString
    defformat = BC8.pack $ iso8601DateFormat Nothing

------------- CSV Helper functions -------------------------

-- Read a word (delimited by a fieldseparator)
word :: Parser ByteString
word = takeWhile1 (\x->(not $ isSpace_w8 x) && (notInClass "," x))

-- Consume the field separator
fieldSeparator :: Parser ByteString
fieldSeparator = takeWhile1 (\x-> isSpace_w8 x || (inClass "," x))

-- Return the value parsed by p, after consuming the field separator
field :: Parser a -> Parser a
field p = do
  val <- p
  fieldSeparator
  return val