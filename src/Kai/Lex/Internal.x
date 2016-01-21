{
{-# LANGUAGE OverloadedStrings #-}
module Kai.Lex.Internal where

import qualified Language.Lua.Token as Lua
import qualified Language.Lua.Syntax as Lua
import Control.Monad.State
import Control.Monad.Except

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BS (w2c)
import qualified Data.ByteString.Lazy.Char8 as C8

import Data.Word (Word8)
import Data.Int (Int64)
import Data.List (foldl1')

import qualified Data.Foldable as F

import Data.Loc
import Data.Monoid ((<>))

import Data.Sequence ((|>),(<|), ViewL(..), ViewR(..))
import qualified Data.Sequence as S

import Kai.LP
import {-# SOURCE #-} Kai.Parse ( parseCall, parseTyping )

import Debug.Trace

}

$digit      = 0-9
$hex        = [0-9 a-f A-F]
$large      = [A-Z \xc0-\xd6 \xd8-\xde]
$small      = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha      = [$small $large]

@luaname    = [$alpha \_] [$alpha $digit \_]*
@inlinename = [$alpha] [$alpha $digit]*

@exponent   = [Ee] ("+" | "-")? $digit+
@hexponent  = [Pp] ("+" | "-")? $hex+
@luaint     = $digit+ | 0 [Xx] $hex+
@luafloat   = $digit+ @exponent | ( $digit* "." $digit+ | $digit+ "." $digit* ) @exponent?
            | 0 [Xx] ($hex+ @hexponent | ( $hex* "." $hex+ | $hex+ "." $hex* ) @hexponent?)

$end = \000
$any = \000-\377

@escape     = \\ ([0abfnrtv\\\"\'] | "x" $hex{2} | $digit{3} | "u{" $hex{3} \} )

@luachar1   = [^ \\ \'] | @escape
@luachar2   = [^ \\ \"] | @escape

$nl = [\n\r]
$horizontalwhite = $white # $nl
@space = $horizontalwhite+ | $horizontalwhite* $nl $horizontalwhite*
@newpar = ($horizontalwhite* $nl){2,} $horizontalwhite*
@letter = [^\{\} $white \\ \[\]]
@math = [^\{\} $white \\ \[\]]

kai :-

                                        "--".*                  { skip }

<typing, typingshort>                   \{                      { descend lua (mkT $ TkLua Lua.TkLBrace) }
<lua>                                   \}                      { ascend (mkT $ TkLua Lua.TkRBrace) }

<typing, typingshort>                   \<                      { descend typing (mkT $ TkLua Lua.TkGt) }
<typing, typingshort>                   \>                      { ascend (mkT $ TkLua Lua.TkGt) }

<typing, typingshort>                   \[                      { descend math (mkT $ TkLua Lua.TkLBracket) }
<math>                                  \]                      { ascend (mkT $ TkLua Lua.TkRBracket) }

<typing>                                @space                  { mkT TkSpace }
<typing>                                @newpar                 { mkT TkNewPar }
<typingshort>                           $horizontalwhite+       { mkT TkSpace }
<typing, typingshort>                   @letter                 { mkTBS TkSymbol }

<math>                                  $white                  { skip }
<math>                                  "^"                     { mkT $ TkLua Lua.TkExponent }
<math>                                  "_"                     { mkT TkUnderscore }
<math>                                  \{                      { descend math (mkT $ TkLua Lua.TkLBrace) }
<math>                                  \}                      { ascend (mkT $ TkLua Lua.TkRBrace) }
<math>                                  @math                   { mkTBS TkSymbol }

<lua, paren, brace, bracket>            $white                  { skip }
<lua, paren, brace, bracket>            @luaint                 { mkTBS $ \s -> TkLua (Lua.TkIntLit $ C8.unpack s) }
<lua, paren, brace, bracket>            @luafloat               { mkTBS $ \s -> TkLua (Lua.TkFloatLit $ C8.unpack s) }
<lua, paren, brace, bracket>            "and"                   { mkT $ TkLua Lua.TkAnd }
<lua, paren, brace, bracket>            "break"                 { mkT $ TkLua Lua.TkBreak }
<lua, paren, brace, bracket>            "do"                    { mkT $ TkLua Lua.TkDo }
<lua, paren, brace, bracket>            "else"                  { mkT $ TkLua Lua.TkElse }
<lua, paren, brace, bracket>            "elseif"                { mkT $ TkLua Lua.TkElseif }
<lua, paren, brace, bracket>            "end"                   { mkT $ TkLua Lua.TkEnd }
<lua, paren, brace, bracket>            "false"                 { mkT $ TkLua Lua.TkFalse }
<lua, paren, brace, bracket>            "for"                   { mkT $ TkLua Lua.TkFor }
<lua, paren, brace, bracket>            "function"              { mkT $ TkLua Lua.TkFunction }
<lua, paren, brace, bracket>            "goto"                  { mkT $ TkLua Lua.TkGoto }
<lua, paren, brace, bracket>            "if"                    { mkT $ TkLua Lua.TkIf }
<lua, paren, brace, bracket>            "in"                    { mkT $ TkLua Lua.TkIn }
<lua, paren, brace, bracket>            "local"                 { mkT $ TkLua Lua.TkLocal }
<lua, paren, brace, bracket>            "nil"                   { mkT $ TkLua Lua.TkNil }
<lua, paren, brace, bracket>            "not"                   { mkT $ TkLua Lua.TkNot }
<lua, paren, brace, bracket>            "or"                    { mkT $ TkLua Lua.TkOr }
<lua, paren, brace, bracket>            "repeat"                { mkT $ TkLua Lua.TkRepeat }
<lua, paren, brace, bracket>            "return"                { mkT $ TkLua Lua.TkReturn }
<lua, paren, brace, bracket>            "then"                  { mkT $ TkLua Lua.TkThen }
<lua, paren, brace, bracket>            "true"                  { mkT $ TkLua Lua.TkTrue }
<lua, paren, brace, bracket>            "until"                 { mkT $ TkLua Lua.TkUntil }
<lua, paren, brace, bracket>            "while"                 { mkT $ TkLua Lua.TkWhile }
<lua, paren, brace, bracket>            "+"                     { mkT $ TkLua Lua.TkPlus }
<lua, paren, brace, bracket>            "-"                     { mkT $ TkLua Lua.TkDash }
<lua, paren, brace, bracket>            "*"                     { mkT $ TkLua Lua.TkMult }
<lua, paren, brace, bracket>            "/"                     { mkT $ TkLua Lua.TkFloatDiv }
<lua, paren, brace, bracket>            "%"                     { mkT $ TkLua Lua.TkModulo }
<lua, paren, brace, bracket>            "^"                     { mkT $ TkLua Lua.TkExponent }
<lua, paren, brace, bracket>            "#"                     { mkT $ TkLua Lua.TkLength }
<lua, paren, brace, bracket>            "&"                     { mkT $ TkLua Lua.TkBitwiseAnd }
<lua, paren, brace, bracket>            "~"                     { mkT $ TkLua Lua.TkTilde }
<lua, paren, brace, bracket>            "|"                     { mkT $ TkLua Lua.TkBitwiseOr }
<lua, paren, brace, bracket>            "<<"                    { mkT $ TkLua Lua.TkLShift }
<lua, paren, brace, bracket>            ">>"                    { mkT $ TkLua Lua.TkRShift }
<lua, paren, brace, bracket>            "//"                    { mkT $ TkLua Lua.TkFloorDiv }
<lua, paren, brace, bracket>            "=="                    { mkT $ TkLua Lua.TkEq }
<lua, paren, brace, bracket>            "~="                    { mkT $ TkLua Lua.TkNeq }
<lua, paren, brace, bracket>            "<="                    { mkT $ TkLua Lua.TkLeq }
<lua, paren, brace, bracket>            ">="                    { mkT $ TkLua Lua.TkGeq }
<lua, paren, brace, bracket, func>      "<" / $nl               { handleLAngle typing }
<lua, paren, brace, bracket, func>      "<"                     { handleLAngle typingshort }
<lua, paren, brace, bracket>            ">"                     { mkT $ TkLua Lua.TkGt }
<lua, paren, brace, bracket>            "="                     { mkT $ TkLua Lua.TkAssign }
<lua, paren, brace, bracket, func>      "("                     { descend paren (mkT $ TkLua Lua.TkLParen) }
<     paren                >            ")"                     { ascend (mkT $ TkLua Lua.TkRParen) }
<lua, paren, brace, bracket, func>      "{"                     { descend brace (mkT $ TkLua Lua.TkLBrace) }
<            brace         >            "}"                     { ascend (mkT $ TkLua Lua.TkRBrace) }
<lua, paren, brace, bracket, func>      "["                     { descend bracket (mkT $ TkLua Lua.TkLBracket) }
<                   bracket>            "]"                     { ascend (mkT $ TkLua Lua.TkRBracket) }
<lua, paren, brace, bracket>            "::"                    { mkT $ TkLua Lua.TkLabel }
<lua, paren, brace, bracket>            ";"                     { mkT $ TkLua Lua.TkSemi }
<lua, paren, brace, bracket, func>      ":"                     { mkT $ TkLua Lua.TkColon }
<lua, paren, brace, bracket>            ","                     { mkT $ TkLua Lua.TkComma }
<lua, paren, brace, bracket, func>      "."                     { mkT $ TkLua Lua.TkDot }
<lua, paren, brace, bracket>            ".."                    { mkT $ TkLua Lua.TkConcat }
<lua, paren, brace, bracket>            "..."                   { mkT $ TkLua Lua.TkVararg }
<lua, paren, brace, bracket, func>      "'"                     { descend string1 (mkT $ TkLua Lua.TkQuote) }
<lua, paren, brace, bracket, func>      \"                      { descend string2 (mkT $ TkLua Lua.TkDoubleQuote) }
<lua, paren, brace, bracket>            @luaname                { mkTBS $ \s -> TkLua (Lua.TkIdent $ C8.unpack s) }
<func>                                  @inlinename             { mkTBS $ \s -> TkLua (Lua.TkIdent $ C8.unpack s) }

<string1>                               @luachar1*              { mkTBS $ \s -> TkLua (Lua.TkStringLit $ C8.unpack s) }
<string1>                               \'                      { ascend (mkT $ TkLua Lua.TkQuote) }
<string2>                               @luachar2*              { mkTBS $ \s -> TkLua (Lua.TkStringLit $ C8.unpack s) }
<string2>                               \"                      { ascend (mkT $ TkLua Lua.TkDoubleQuote) }

{


type LexerAction = SourceFeed -> Int64 -> LP (L Token)

mkTBS :: (BS.ByteString -> Token) -> LexerAction
mkTBS t feed len = do
    feed' <- gets sourceFeed
    return $ L (Loc (feedPosition feed) (feedPosition feed')) (t $ C8.take len (feedData feed))

mkT :: Token -> LexerAction
mkT t = mkTBS (const t)

afterBegin :: LexerAction -> SC -> LexerAction
afterBegin la sc input len = do
    modify' $ \st -> st {currentSC = sc}
    la input len

descend :: SC -> LexerAction -> LexerAction
descend sc con input len = do
    sc' <- gets currentSC
    modify' $ \st -> st 
        { pathSC = sc' : pathSC st
        , currentSC = sc
        }
    con input len

ascend :: LexerAction -> LexerAction
ascend con input len = do
    bc <- gets pathSC
    case bc of
        [] -> lpErr "illegal end block character"
        sc:scs -> do
            modify' $ \st -> st
                { pathSC = scs
                , currentSC = sc
                }
            con input len

scanAll :: LP [L Token]
scanAll = F.toList <$> go S.empty
  where 
    go ts = do
        t <- scan
        case t of
            L _ TkEOF -> return (ts |> t)
            _ -> go (ts |> t)

handleLAngle :: SC -> LexerAction
handleLAngle sc feed len = do
    st <- get
    put $ st { pathSC = [currentSC st], currentSC = sc }
    (do
        ts <- getTs S.empty
        st' <- get
        put $ st' { accumTokens = ts, sourceFeed = emptyFeed }
        t <- parseTyping
        modify' $ \s -> s { pathSC = pathSC st, currentSC = currentSC st, sourceFeed = sourceFeed st',
            accumTokens = S.fromList $ map (L NoLoc) [TkLua Lua.TkGt, TkTyping t, TkLua Lua.TkLt]}
        scan
        ) `catchError` const (do
            put st
            (mkT $ TkLua Lua.TkLt) feed len
            )
getTs ts = do
    t <- scan
    p <- gets pathSC
    case p of
        [] -> return ts
        _ -> case t of
            L _ TkEOF -> lpErr "Couldn't close angle bracket"
            _ -> getTs (t <| ts)

scan :: LP (L Token)
scan = do
    ts <- gets accumTokens
    case S.viewr ts of
        ts':>t -> do
            modify' $ \st -> st { accumTokens = ts' }
            return t
        EmptyR -> do
            ip <- inlinePossible
            case ip of
                True -> scanInline `catchError` const scanNormal
                False -> scanNormal
  where
    inlinePossible = do
        sc <- gets currentSC
        return $ sc `elem` [typing, typingshort, math]


scanNormal :: LP (L Token)
scanNormal = do
    feed <- gets sourceFeed
    sc <- gets currentSC
    case alexScan feed sc of
        AlexEOF -> return $ L NoLoc TkEOF
        AlexError feed' -> lpErr $ "Unexpected character (startcode = " <> C8.pack (show sc) <> ")"
        AlexSkip feed' len -> do
            modify' $ \st -> st { sourceFeed = feed' }
            scan
        AlexToken feed' len action -> do
            modify' $ \st -> st { sourceFeed = feed' }
            action feed (fromIntegral len)

-- finds the largest valid inline function call (if there is one)
scanInline :: LP (L Token)
scanInline = do
    feed <- gets sourceFeed
    st <- get
    put $ st { currentSC = func, pathSC = [] }
    tfs <- getTs []
    (t,st') <- valid st tfs
    put $ st' { currentSC = currentSC st, pathSC = pathSC st }
    return t
  where 
    valid s [] = lpErr "no valid inline calls"
    valid s ((t, Nothing):tfs) = valid s tfs
    valid s tfs@((t, Just st):tfs') = (do
        put $ s { sourceFeed = emptyFeed, accumTokens = (S.fromList $ map fst tfs) }
        x <- parseCall
        return $ 
            let l = foldl1' (<-->) . map (\(L l _, _) -> l) $ tfs 
            in (L l (TkInline x), st)
        ) `catchError` const (valid s tfs')
            
    getTs tfs = (do
        accumLength <- gets (S.length . accumTokens)
        if accumLength == 1 then do
                t :< _ <- gets (S.viewl . accumTokens)
                modify' $ \st -> st { accumTokens = S.empty }
                mst <- gets Just 
                getTs ((t,mst):tfs)
            else if accumLength > 1 then do
                t <- scan
                getTs ((t,Nothing):tfs)
            else do
                t <- scan
                p <- gets pathSC
                case p of
                    [] -> case t of
                        L _ (TkLua _) -> do
                            st <- get
                            getTs ((t, Just st):tfs)
                        _ -> return tfs
                    _ -> case t of
                        L _ TkEOF -> return tfs
                        _ -> getTs ((t, Nothing):tfs)
        ) `catchError` const (return tfs)

skip _ _ = scan

-- Alex compatability

type AlexInput = SourceFeed
alexGetByte :: SourceFeed -> Maybe (Word8, SourceFeed)
alexGetByte feed 
    | BS.null (feedData feed) = Nothing
    | otherwise = let 
            b = BS.head (feedData feed)
            d = BS.tail (feedData feed)
            c = BS.w2c b
            p = alexMove (feedPosition feed) c
        in p `seq` d `seq` Just (b, SourceFeed
            { feedPosition = p
            , feedData = d
            , feedPrevChar = c
            })
alexInputPrevChar :: SourceFeed -> Char
alexInputPrevChar = feedPrevChar

tab_size = 4

alexMove :: Pos -> Char -> Pos
alexMove (Pos f l c a) '\t' = Pos f l (((c+tab_size-1) `div` tab_size)*tab_size+1) (a+1)
alexMove (Pos f l c a) '\n' = Pos f (l+1) 1 (a+1)
alexMove (Pos f l c a) _    = Pos f l (c+1) (a+1)

}
