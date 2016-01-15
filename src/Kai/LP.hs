{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

module Kai.LP where

import Control.Monad.State
import Control.Monad.Except

import qualified Language.Lua.Token as Lua
import qualified Language.Lua.Syntax as Lua
import qualified Language.Lua.Parser as Lua

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Word (Word8)

import Data.Int (Int64)

import qualified Data.Sequence as S

import Data.Loc

import Text.PrettyPrint

type SC = Int

data CompileMsg = CompileMsg { msgLocation :: Loc, msgDescription :: BS.ByteString }
    deriving (Show, Eq)
emptyCompileMsg = CompileMsg { msgLocation = NoLoc, msgDescription = "" }

prettyCompileMsg :: CompileMsg -> BS.ByteString -> Doc
prettyCompileMsg m src = case msgLocation m of 
    NoLoc -> text (C8.unpack (msgDescription m))
    l@(Loc (Pos f l1 c1 o1) (Pos _ _ c2 o2)) -> 
        let padding = 5
            paddingStart = min c1 padding
        in hcat
            [ text $ displayLoc l, text ": "
            , text $ C8.unpack (msgDescription m), text ": "
            , nest 3 . vcat $
                [ text $ C8.unpack . C8.takeWhile (/='\n') . C8.take (fromIntegral $ o2 - o1 + 1 + padding + paddingStart) . C8.drop (fromIntegral $ o1 - padding) $ src
                , nest paddingStart . text $ replicate (o2-o1+1) '~'
                ]
            ]

data Token 
  = TkLua Lua.Token
  | TkChar BS.ByteString
  | TkInline (Lua.FunctionCall ())
  | TkNewPar
  | TkSpace
  | TkSymbol BS.ByteString
  | TkUnderscore
  | TkEOF
    deriving (Show, Eq)

data LPState = LPState
  { sourceFeed :: SourceFeed
  , currentSC :: !SC
  , pathSC :: [SC]
  , accumTokens :: S.Seq (L Token)
  }
    deriving (Show, Eq)

initialLPState = LPState
  { sourceFeed = emptyFeed
  , currentSC = 0
  , pathSC = []
  , accumTokens = S.empty
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

runLPList :: LP a -> SC -> [L Token] -> Either CompileMsg a
runLPList (LP e) sc ts = evalState (runExceptT e) (initialLPState { accumTokens = S.fromList ts })

lpErr :: BS.ByteString -> LP a
lpErr d = do
    p <- gets (feedPosition . sourceFeed)
    throwError $ emptyCompileMsg { msgLocation = fromPos p, msgDescription = d }

data SourceFeed = SourceFeed
  { feedPosition :: !Pos
  , feedPrevChar :: !Char
  , feedData :: BS.ByteString
  }
    deriving (Show, Eq)
emptyFeed = SourceFeed
  { feedPosition = startPos ""
  , feedPrevChar = '\n'
  , feedData = ""
  }
