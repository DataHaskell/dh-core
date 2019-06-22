{- |
Module      :  DebugTrace
Description :  Enables debug-traces that help in tracing values
Copyright   :  (c) Arvind Devarajan
License     :  MIT

Maintainer  :  arvindd
Stability   :  unstable
Portability :  portable
-}
module DebugTrace
    ( debug
    , debugM
    , debugPeekChar
    , debugEatChar
    ) where

import Data.Attoparsec.ByteString.Lazy (Parser) 
import Data.Attoparsec.ByteString.Char8 (anyChar, peekChar')   
import Debug.Trace (trace, traceM)

-- Make False to True to switch on tracing
enableDebugTrace :: Bool
enableDebugTrace = False

-- Trace with value return
debug s v | enableDebugTrace  = trace s v
          | otherwise  = v        

-- Trace inside a do block: make False to True to switch on tracing          
debugM :: (Applicative f) => 
  String  -- Message that is printed
  -> f ()
debugM s | enableDebugTrace = traceM s   
         | otherwise  = pure () 

-- Peek next char and print
debugPeekChar :: 
     String  -- Message that is printed along with next char
  -> Parser () 
debugPeekChar s = do
  c <- peekChar'
  debugM $ s ++ ": " ++ [c]

-- Eatup next char and print it  
debugEatChar ::
     String -- Message that is printed along with next char
  -> Parser ()
debugEatChar s = do
  c <- anyChar
  debugM $ s ++ ": " ++ [c]