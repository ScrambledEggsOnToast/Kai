{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

module Kai.LP where

import Control.Monad.State
import Control.Monad.Except

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Word (Word8)

import Data.Int (Int64)

type SC = Int

data CompileMsg = CompileMsg { msgPosition :: SourcePosition, msgDescription :: BS.ByteString }
    deriving Show
emptyCompileMsg = CompileMsg { msgPosition = startPosition, msgDescription = "" }

prettyCompileMsg :: CompileMsg -> String
prettyCompileMsg msg = (show (lineNumber $ msgPosition msg)) ++ ":" ++ (show (lineNumber $ msgPosition msg)) ++ ": " ++ C8.unpack (msgDescription msg)

data SourcePosition = SourcePosition { charOffset :: !Int64, lineNumber :: !Int64, columnNumber :: !Int64 }
    deriving (Show, Eq)

startPosition = SourcePosition 0 1 1

data LPState = LPState
  { sourceFeed :: SourceFeed
  , currentSC :: !SC
  , pathSC :: [SC]
  }
    deriving (Show, Eq)

initialLPState = LPState
  { sourceFeed = emptyFeed
  , currentSC = 0
  , pathSC = []
  }

newtype LP a = LP { unLP :: ExceptT CompileMsg (State LPState) a }

instance Functor LP where
    fmap f (LP m) = LP (fmap f m)
instance Applicative LP where
    pure = LP . pure
    (LP f) <*> (LP x) = LP (f <*> x)
instance Monad LP where
    return = LP . return
    (LP m) >>= f = LP (m >>= unLP . f)
instance MonadState LPState LP where
    get = LP get
    put = LP . put
instance MonadError CompileMsg LP where
    throwError = LP . throwError
    catchError (LP m) f = LP (catchError m (unLP . f))

runLP :: LP a -> SC -> BS.ByteString -> Either CompileMsg a
runLP (LP e) sc inp = evalState (runExceptT e) (initialLPState { sourceFeed = emptyFeed { feedData = inp }, currentSC = sc })

lpErr :: BS.ByteString -> LP a
lpErr d = do
    p <- gets (feedPosition . sourceFeed)
    throwError $ emptyCompileMsg { msgPosition = p, msgDescription = d }

data SourceFeed = SourceFeed
  { feedPosition :: !SourcePosition
  , feedPrevChar :: !Char
  , feedData :: BS.ByteString
  }
    deriving (Show, Eq)
emptyFeed = SourceFeed
  { feedPosition = startPosition
  , feedPrevChar = '\n'
  , feedData = ""
  }

data Positioned a = Pos SourcePosition a
    deriving (Show, Eq)
position :: Positioned a -> SourcePosition
position (Pos p _) = p
stripPosition :: Positioned a -> a
stripPosition (Pos _ a) = a

instance Functor Positioned where
    fmap f (Pos p x) = Pos p (f x)
