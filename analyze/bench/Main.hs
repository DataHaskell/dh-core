{-# LANGUAGE GADTs #-}
module Main where

import           Streamly
import qualified Streamly.Prelude as S

import Control.Monad.Catch (MonadThrow (..))
import Data.Function ((&))
import Control.Monad
import System.IO.Unsafe

import Analyze.Common (Data (..), DuplicateKeyError (..), RowSizeMismatch (..))
import qualified Analyze as A
import qualified Analyze.Common as AC


import qualified Data.Vector         as V
import           Data.Vector         (Vector)

import qualified Data.Text           as T
import           Data.Text           (Text)

import           Data.HashMap.Strict (HashMap)
-- import qualified Data.HashMap.Strict as HM
-- import qualified Data.HashSet        as HS

import System.Random (randomRs, newStdGen)
import qualified System.Random.MWC as M
import qualified Criterion.Main as C

import qualified Weigh as W

main :: IO ()
main = do
  speed
  -- weight

-- implementation details required
data RFrameUpdateS k v m where
    RFrameUpdateS :: MonadThrow m => { 
    _rframeUpdateKeys :: !(Vector k),
    _rframeUpdateData :: !(SerialT m (Vector v))
    } -> RFrameUpdateS k v m 

data RFrameS k v m where
    
    RFrameS :: MonadThrow m => { 
    _rframeKeys :: !(Vector k),
    _rframeLookup :: !(HashMap k Int),
    _rframeData   :: !(SerialT m (Vector v))
    } -> RFrameS k v m


fromUpdate :: (Data k, MonadThrow m) => RFrameUpdateS k v m -> m (RFrameS k v m)
fromUpdate (RFrameUpdateS ks vs) = AC.checkForDupes ks >> pure (RFrameS ks (AC.makeLookup ks) vs)

update :: (Data k, MonadThrow m) => RFrameUpdateS k v m -> RFrameS k v m -> m (RFrameS k v m)
update (RFrameUpdateS uks uvs) (RFrameS fks _ fvs) = do
  fSize <- S.length fvs
  uSize <- S.length uvs
  if fSize /= uSize
    then throwM (RowSizeMismatch fSize uSize)
    else do
      AC.checkForDupes uks
      let kis = AC.mergeKeys fks uks
          ks' = (\(k, _, _) -> k) <$> kis
          look' = AC.makeLookup ks'
          vs' = S.zipWith (AC.runIndexedLookup kis) fvs uvs
      return (RFrameS ks' look' vs')


-- only concerned with generating data
n :: Int
n = 1500

testKeys :: Vector Text
testKeys = unsafePerformIO $ V.replicateM n $ liftM (T.pack . take 10 . randomRs ('a','z')) newStdGen

testData :: Vector (Vector Double)
testData = unsafePerformIO $ do 
    gen <- M.create
    V.replicateM n $ M.uniformVector gen n

testDataS :: SerialT IO (Vector Double)
testDataS = unsafePerformIO $  do
    let
      vec = testData
    return $ S.fromFoldable vec

-- the actual benchmarks
cmprVec :: IO Bool
cmprVec = do
    let 
        keys = testKeys
        dat = testData
        upd = A.RFrameUpdate keys dat
    frame1 <- A.fromUpdate upd
    frame2 <- A.fromUpdate upd
    return $ frame1 == frame2

-- compr fills the same role as (==) but wrapped in IO
cmpr :: RFrameS Text Double IO -> RFrameS Text Double IO -> IO Bool
cmpr f1 f2 = do
    let 
        sameKeys = _rframeKeys f1 == _rframeKeys f2
        sameLookup = _rframeLookup f1 == _rframeLookup f2
        dat1 = _rframeData f1 
        dat2 = _rframeData f2
    sameDat <- (S.zipWith (==) dat1 dat2 & S.notElem False)
    return $ sameKeys && sameLookup && sameDat 

cmprStream :: IO Bool
cmprStream = do
    let 
        keys = testKeys
        dat = testDataS
        upd = RFrameUpdateS keys dat
    frame1 <- fromUpdate upd
    frame2 <- fromUpdate upd
    cmpr frame1 frame2

takeRowsVec :: Int -> IO Bool
takeRowsVec m = do
    let 
        keys = testKeys
        dat = testData
        upd = A.RFrameUpdate keys dat
    frame <- A.fromUpdate upd
    let 
        postTake = A.takeRows m frame
    return $ (==) frame postTake  -- probably returns false, it's just to force evaluation

-- | Takes first 'n' rows of an 'RFrameS'.
takeRows :: Int -> RFrameS Text Double IO -> RFrameS Text Double IO  
takeRows n (RFrameS ks look srm) = RFrameS ks look (S.take n srm)

takeRowsS :: Int -> IO Bool
takeRowsS m = do
    let 
        keys = testKeys
        dat = testDataS
        update = RFrameUpdateS keys dat
    frame <- fromUpdate update
    let 
        postTake = takeRows m frame
    cmpr frame postTake 

speed :: IO ()
speed = do
    -- cmprVec -- to actually generate the data
    -- cmprStream -- this too
    C.defaultMain [
      C.bgroup "Tests" [
          C.bench "Vec" $ C.whnfIO cmprVec    
          , C.bench "Stream" $ C.whnfIO cmprStream
          , C.bench "takeRowsVec 1000" $ C.whnfIO (takeRowsVec 1000)
          , C.bench "takeRowsVec 500" $ C.whnfIO (takeRowsVec 500)
          , C.bench "takeRowsVec 200" $ C.whnfIO (takeRowsVec 200)
          , C.bench "takeRowsVec 100" $ C.whnfIO (takeRowsVec 100)
          , C.bench "takeRowsStream 1000" $ C.whnfIO (takeRowsS 1000)
          , C.bench "takeRowsStream 500" $ C.whnfIO (takeRowsS 500)
          , C.bench "takeRowsStream 200" $ C.whnfIO (takeRowsS 200)
          , C.bench "takeRowsStream 100" $ C.whnfIO (takeRowsS 100)]]

weight :: IO ()
weight = do
    -- cmprVec
    -- cmprStream 
    W.mainWith ( do
                   W.action "Vec" cmprVec    
                   W.action "Stream"  cmprStream
                 --     W.io "takeRowsVec 1000"  takeRowsVec 1000
                   W.io "takeRowsVec 500"  takeRowsVec 500
                   W.io "takeRowsVec 200"  takeRowsVec 200
                   W.io "takeRowsVec 100"  takeRowsVec 100
                 --      W.io "takeRowsStream 1000"  takeRowsS 1000
                   W.io "takeRowsStream 500"  takeRowsS 500
                   W.io "takeRowsStream 200"  takeRowsS 200
                   W.io "takeRowsStream 100" takeRowsS 100)
