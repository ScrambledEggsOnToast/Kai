{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

module Kai.LP where

import Kai.Syntax

import Control.Monad.State
import Control.Monad.Except

import qualified Language.Lua.Token as Lua
import Language.Lua.Syntax hiding (Block)
import qualified Language.Lua.Parser as Lua

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Word (Word8)

import Data.Int (Int64)

import qualified Data.Sequence as S
import qualified Data.Foldable as F

import Data.Loc

import qualified Text.PrettyPrint.Leijen as PP

type SC = Int

data CompileMsg = CompileMsg { msgLocation :: Loc, msgDescription :: BS.ByteString }
    deriving (Show, Eq)
emptyCompileMsg = CompileMsg { msgLocation = NoLoc, msgDescription = "" }

prettyCompileMsg :: CompileMsg -> BS.ByteString -> PP.Doc
prettyCompileMsg m src = case msgLocation m of 
    NoLoc -> PP.text (U.toString (msgDescription m))
    l@(Loc (Pos f l1 c1 o1) (Pos _ _ c2 o2)) -> 
        let padding = 5
            paddingStart = min c1 padding
            pretext = displayLoc l ++ ": " ++ U.toString (msgDescription m) ++ ": "
        in PP.hcat
            [ PP.text pretext
            , PP.nest (length pretext) . PP.vcat $
                [ PP.text $ U.toString . head . U.lines . U.take (fromIntegral $ o2 - o1 + 1 + padding + paddingStart) . U.drop (fromIntegral $ o1 - padding) $ src
                , PP.text $ replicate paddingStart ' ' ++ replicate (o2-o1+1) '~'
                ]
            ]

data Token 
  = TkLua Lua.Token
  | TkChar BS.ByteString
  | TkInline (FunctionCall ())
  | TkTyping (FunctionCall ())
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

data Builder a = Builder { runBuilder :: S.Seq (KaiLua ()) -> Int -> (a, S.Seq (KaiLua ()), S.Seq (Typing ())) }
instance Functor Builder where
    fmap f (Builder tb) = Builder $ \luas tCount -> 
        let (x, luas', typings) = tb luas tCount in (f x, luas', typings)
instance Applicative Builder where
    pure x = Builder $ \luas _ -> (x, luas, S.empty)
    Builder tbf <*> Builder tbx = Builder $ \luas tCount ->
        let (f, luas', typings) = tbf luas tCount
            (x, luas'', typings') = tbx luas' (tCount + S.length typings)
        in (f x, luas'', typings S.>< typings')
instance Monad Builder where
    return = pure
    Builder tbx >>= f = Builder $ \luas tCount ->
        let (x, luas', typings) = tbx luas tCount
            Builder tby = f x
            (y, luas'', typings') = tby luas' (tCount + S.length typings)
        in (y, luas'', typings S.>< typings')

getArgs :: LP (S.Seq (KaiLua ()), Int)
getArgs = LP . lift . lift . Builder $ \luas tCount -> ((luas, tCount), luas, S.empty)

insertArgs :: (S.Seq (KaiLua ()), Int) -> LP a -> LP a
insertArgs (luas, tCount) (LP e) = do
    lpSt <- get
    let s = runExceptT e
        b = runStateT s lpSt
        ((ex, st), ls, ts) = runBuilder b luas tCount
    LP . ExceptT . StateT $ \_ -> Builder $ \_ _ -> ((ex,st),ls,ts)

newLua :: KaiLua () -> LP LuaRef
newLua l = LP . lift . lift . Builder $ \luas _ -> (LuaRef (S.length luas), luas S.|> l, S.empty)

makeCall :: Typing () -> LP (FunctionCall ())
makeCall t = LP . lift . lift . Builder $ \luas tCount ->
    let args = mkArg <$> luas
    in (mkTypingCall tCount args, S.empty, S.singleton t)
  where
    mkArg (Call _ fc) = PrefixExp () (PrefixFunCall () fc)
    mkArg (Block _ b) = 
        PrefixExp () (
            PrefixFunCall () (
                FunctionCall () (
                    Parens () (
                        FunDef () (
                            FunctionBody () (IdentList () []) False b
                        )
                    )
                ) ( 
                    Args () (ExpressionList () [])
                )
            )
        )
    mkTypingCall r as =
        FunctionCall () (
            PrefixVar () (VarFieldName () (PrefixVar () (VarIdent () (Ident () "__Kai"))) (Ident () "typing")))
            (Args () (ExpressionList () ((Integer () (show r)) : F.toList as)))
    

newtype LP a = LP { unLP :: ExceptT CompileMsg (StateT LPState Builder) a }

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
    catchError (LP m) f = do
        st <- get
        as <- getArgs
        let d e = unLP $ insertArgs as (f e)
        LP (catchError m (\e -> do
            put st 
            d e))

runLP :: LP a -> SC -> BS.ByteString -> Either CompileMsg (a, S.Seq (Typing ()))
runLP (LP e) sc inp = 
    let st = runExceptT e
        b = evalStateT st (initialLPState { sourceFeed = emptyFeed { feedData = inp }, currentSC = sc })
        (ex, l, ts) = runBuilder b S.empty 0
    in do
        x <- ex
        return (x,ts)

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
