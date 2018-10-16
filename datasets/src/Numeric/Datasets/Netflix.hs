{-# language OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|
Netflix prize dataset

From the README : The movie rating files contain over 100 million ratings from 480 thousand randomly-chosen, anonymous Netflix customers over 17 thousand movie titles.  The data were collected between October, 1998 and December, 2005 and reflect the distribution of all ratings received during this period.  The ratings are on a scale from 1 to 5 (integral) stars. To protect customer privacy, each customer id has been replaced with a randomly-assigned id.  The date of each rating and the title and year of release for each movie id are also provided.

The competition ended on September, 2009, and the dataset was subsequently removed from the public domain by the company (see <http://netflixprize.com/>).

We include in the repository a tiny subset of the original dataset for development purposes. Since we use `file-embed` to load the data, the directories are hardcoded (see the Datasets section below); users may either symlink or copy the full dataset in the given directories.

-}

module Numeric.Datasets.Netflix (
  -- * Dataset parsing and shaping
  parseTrainingSet, parseTestSet, parseMovies,
  -- * Types
  RD(..),
  UserId, MovieId,
  Train(..), Test(..), Movie(..),
  RatingDate(..),
  -- * Datasets
  trainingSet, testSet, movies
  ) where

import Prelude hiding (takeWhile)

import Numeric.Datasets
-- import Data.Csv
import Data.FileEmbed
import Data.ByteString hiding (map, head, takeWhile)
import Data.Time (Day, fromGregorian)

-- import Control.Applicative
import Data.Monoid (mconcat)
import Data.Traversable (traverse)
import qualified Data.Attoparsec.Internal.Types as PT (Parser)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 hiding (takeWhile, inClass)



-- * Dataset files

-- The directories are scanned recursively and their contents are presented as (FilePath, ByteString) pairs

-- | The training set (a set of text files) is assumed to be in the directory `datafiles/netflix/training/` relative to the repository root
trainingSet :: [(FilePath, ByteString)]
trainingSet = $(embedDir "datafiles/netflix/training/")

-- | The test set (one text file) is assumed to be in the directory `datafiles/netflix/test/` relative to the repository root
testSet :: [(FilePath, ByteString)]
testSet = $(embedDir "datafiles/netflix/test/")

-- | The movies dataset (one text file) is assumed to be in the directory `datafiles/netflix/movies/` relative to the repository root
movies :: [(FilePath, ByteString)]
movies = $(embedDir "datafiles/netflix/movies/")



-- * Data types

-- | A date-tagged movie rating
data RatingDate = RatingDate {userId :: UserId,
                              ratingDate :: Day} deriving (Eq, Show)

-- | User ID (anonymized)
newtype UserId = UserId {unUserId :: Int} deriving Eq
instance Show UserId where show = show . unUserId

-- | Training set item
data Train = Train {trainRating :: RatingDate,
                    rating :: Int } deriving (Eq, Show)

-- | Movie ID
newtype MovieId = MovieId {unMovieId :: Int} deriving Eq
instance Show MovieId where show = show . unMovieId

-- | Movie dataset item
data Movie = Movie { movieId :: MovieId,
                     releaseYear :: Day,
                     movieTitle :: ByteString } deriving (Eq, Show)

-- | Test set item
newtype Test = Test { testRating :: RatingDate } deriving (Eq, Show)




-- * Additional types and helper functions

-- Every file in the training set corresponds to a distinct column. The whole dataset can therefore be seen as a (very sparse) users vs. movies matrix

data Col a = Col {cMovieId :: MovieId,
                  cSet :: [a]} deriving (Eq, Show)

newtype TrainCol = TrainC { unTrC :: Col Train } deriving (Eq, Show)

mkTrainCol :: MovieId -> [Train] -> TrainCol
mkTrainCol mid cs = TrainC (Col mid cs)



newtype TestCol = TestC { unTeC :: Col Test } deriving (Eq, Show)

mkTestCol :: MovieId -> [Test] -> TestCol
mkTestCol mid cs = TestC (Col mid cs)



-- | A type for date-tagged movie ratings
data RD a = RD { rdRating :: a,
                 rdDate :: Day} deriving (Eq, Show)

-- Convert a column of training data into (row, col, (rating, date)) coordinate format suitable for populating a sparse matrix
toCoordsTrainCol :: Num a => TrainCol -> [(UserId, MovieId, RD a)]
toCoordsTrainCol tc = map (f mid) tss where
  tss = cSet $ unTrC tc
  mid = cMovieId $ unTrC tc
  f m ts = (uid, m, RD r d) where
    r = fromIntegral $ rating ts
    d = ratingDate $ trainRating ts
    uid = userId $ trainRating ts

toCoordsTestCol :: TestCol -> [(UserId, MovieId, Day)]
toCoordsTestCol tc = map (f mid) tss where
  tss = cSet $ unTeC tc
  mid = cMovieId $ unTeC tc
  f m ts = (uid, m, d) where
    d = ratingDate $ testRating ts
    uid = userId $ testRating ts


-- | Parse the whole training set, convert to coordinate format and concatenate into a single list.
parseTrainingSet :: Num a => Either String [(UserId, MovieId, RD a)]
parseTrainingSet = mconcat <$> parseTrainingSet'

-- | Parse the whole training set and convert to coordinate format (each dataset file is parsed into a distinct inner list)
parseTrainingSet' :: Num a => Either String [[(UserId, MovieId, RD a)]]
parseTrainingSet' = do
  d <- traverse (parseOnly trainingSetParser . snd) trainingSet
  pure $ map toCoordsTrainCol d


-- | Parse the whole test set, convert to coordinate format and concatenate into a single list.
parseTestSet :: Either String [(UserId, MovieId, Day)]
parseTestSet = mconcat <$> parseTestSet'

-- | Parse the whole test set and convert to coordinate format
parseTestSet' :: Either String [[(UserId, MovieId, Day)]]
parseTestSet' = do
  d <- traverse (parseOnly testSetParser . snd) testSet
  return $ map toCoordsTestCol $ mconcat d

-- | Parse the whole movies file, convert to coordinate format and concatenate into a single list.
parseMovies :: Either String [Movie]
parseMovies = do
  d <- traverse (parseOnly moviesParser . snd) movies
  return $ mconcat d


-- * Netflix dataset parsers

-- | The first line of each training set file contains the movie id followed by a
-- colon.  Each subsequent line in the file corresponds to a rating from a customer
-- and its date in the following format:
--
-- CustomerID,Rating,Date
--
-- - MovieIDs range from 1 to 17770 sequentially.
-- - CustomerIDs range from 1 to 2649429, with gaps. There are 480189 users.
-- - Ratings are on a five star (integral) scale from 1 to 5.
-- - Dates have the format YYYY-MM-DD.
trainingSetParser :: PT.Parser ByteString TrainCol
trainingSetParser = do
  (mid, tr) <- stanza trainRow
  return $ mkTrainCol mid tr

-- | The test set ("qualifying") file consists of lines indicating a movie id, followed by a colon, and then customer ids and rating dates, one per line for that movie id.
-- The movie and customer ids are contained in the training set.  Of course the
-- ratings are withheld. There are no empty lines in the file.
testSetParser :: PT.Parser ByteString [TestCol]
testSetParser = do
  ll <- many1 (stanza testRow)
  return $ map (uncurry mkTestCol) ll


-- | Movie information is in the following format:
--
-- MovieID,YearOfRelease,Title
--
-- - MovieID do not correspond to actual Netflix movie ids or IMDB movie ids.
-- - YearOfRelease can range from 1890 to 2005 and may correspond to the release of
--   corresponding DVD, not necessarily its theaterical release.
-- - Title is the Netflix movie title and may not correspond to
--   titles used on other sites.  Titles are in English.
moviesParser :: PT.Parser ByteString [Movie]
moviesParser = parseRows moviesRow




-- * Netflix dataset row type parsers

trainRow :: PT.Parser ByteString Train
trainRow = do
  uid <- decc
  rate <- decc
  d <- date
  let r = RatingDate (UserId uid) d
  return $ Train r rate

testRow :: PT.Parser ByteString Test
testRow = do
  uid <- decc
  d <- date
  let r = RatingDate (UserId uid) d
  return $ Test r

moviesRow :: PT.Parser ByteString Movie
moviesRow = do
  mo <- decc
  ye <- decc
  title <- takeWhile (inClass "-a-zA-Z0-9 :,&.")
  return $ Movie (MovieId mo) (fromGregorian (fromIntegral ye) 1 1) title





-- * Attoparsec parser combinators

parseRows :: PT.Parser ByteString a -> PT.Parser ByteString [a]
parseRows p = many1 (p <* endOfLine)

-- a "stanza" is a block of rows starting with the movie ID and a colon.
stanza :: PT.Parser ByteString a -> PT.Parser ByteString (MovieId, [a])
stanza p = do
  i <- ident <* endOfLine
  pp <- many1 (p <* endOfLine)
  return (MovieId (fromIntegral i), pp)



-- * Attoparsec helpers

date :: PT.Parser ByteString Day
date = do
  (yy:mm:dd:_) <- sepBy decimal dash
  pure $ fromGregorian (fromIntegral yy) mm dd

comma, dash :: Parser Char
comma = char ','
dash = char '-'

decc :: PT.Parser ByteString Int
decc = do
  d <- decimal
  _ <- comma
  return d

ident :: PT.Parser ByteString Integer
ident = do
  i <- decimal
  _ <- char ':'
  return i





