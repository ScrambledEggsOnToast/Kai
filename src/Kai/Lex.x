{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs -funbox-strict-fields #-}
module Kai.Lex where

import Kai.LP

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BS (w2c)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Word (Word8)

import qualified Data.ByteString.Read as BSR

import Data.Int (Int64)
}

$whitechar = [ \t\n\r\f\v]
$special   = [\,\:\\]

$scriptblock = [\{\}]
$functionargs = [\(\)]

$digit     = 0-9
$symbol    = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\[\]\(\)\{\}] # [$special \_\"\']

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \"\']

$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \-\_]
$symchar   = [$symbol]
$nl        = [\n\r]
$horizontalwhite = $white # $nl

@newpar = ($horizontalwhite* $nl){2} $horizontalwhite*

@reservedid =
    class|import|return|for|break|continue|print

@reservedidblock =
    in | if | else | elseif

@scriptop =
    "+"|"-"|"*"|"/"|"="|"+="|"-="|"*="|"/="|"=="|"<"|">"|"<="|">="|"^"|".."

@mathop = 
    "^"|"_"

@varid     = $alpha $idchar*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal
$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
     | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
     | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
     | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

$comment = \%
$typing = [^$scriptblock $comment $horizontalwhite \\ \[\]]
$math = [^$scriptblock $comment $horizontalwhite \\ \[\]]

kai :-

<typing_sc, script_sc>      $comment[^\{].*                 { skip }

                            "%{"                            { descend comment_sc skip }
<comment_sc>                "}%"                            { ascend skip }
<comment_sc>                [^\}] | \}[^\%]                 { skip }
<comment_sc>                $nl                             { skip }

<typing_sc>                 \{                              { descend script_sc (mkTBS TSpecial) }
<script_sc>                 \{                              { descend struct_sc (mkTBS TSpecial) }
<typing_sc, script_sc>      \}                              { ascend (mkTBS TSpecial) }

<script_sc>                 @reservedid                     { mkTBS TReservedId }
<script_sc>                 @reservedidblock                { descend scripttop_sc (mkTBS TReservedId) }
<script_sc>                 function                        { descend functop_sc (mkTBS TReservedId) }

<script_sc>                 ($horizontalwhite* $nl)+ 
                                $horizontalwhite*           { mkT TNewLine }
<script_sc, struct_sc,
    paren_sc>               $special                        { mkTBS TSpecial }
<script_sc, struct_sc,
    paren_sc>               @varid                          { mkTBS TVarId }
<script_sc, struct_sc,
    paren_sc>               @scriptop                       { mkTBS TReservedOp }
<script_sc, struct_sc, 
    paren_sc>               "-"?@decimal                    { mkTInteger }
<script_sc, struct_sc, 
    paren_sc>               @decimal \. @decimal @exponent? 
                                | @decimal @exponent        { mkTFloat }
<script_sc, struct_sc,
    paren_sc>               "true" | "false"                { mkTBool }
<script_sc, struct_sc,
    paren_sc>               \" @string* \"                  { mkTBS (TString . C8.pack . read . C8.unpack) }
<script_sc, struct_sc,
    paren_sc>               $horizontalwhite+               { skip }
<script_sc, struct_sc,
    paren_sc>               \(                              { descend paren_sc (mkTBS TSpecial) }
<script_sc, struct_sc,
    paren_sc>               \[                              { descend typing_sc (mkTBS TSpecial) }

<typing_sc>                 $horizontalwhite+  
                                | $horizontalwhite* $nl 
                                $horizontalwhite*           { mkT TSpace }
<typing_sc>                 @newpar                         { mkT TNewPar }
<typing_sc>                 $typing / [[^\[\(] $nl]         { mkTLetter }
<typing_sc>                 \[                              { descend math_sc (mkTBS TSpecial) }

<typing_sc, math_sc>        \\ / $alpha                     { mkTBS TSpecial }
<typing_sc, math_sc>        \\^@varid                       { mkTBS TVarId }
<typing_sc, math_sc>        @varid / \(                     { descend paren_sc' (mkTBS TVarId) }
<typing_sc, math_sc>        @varid / \[                     { descend paren_sc' (mkTBS TVarId) }

<paren_sc'>                 \(                              { descend paren_sc (mkTBS TSpecial) }
<paren_sc'>                 \[                              { descend typing_sc (mkTBS TSpecial) }
<paren_sc'>                 $horizontalwhite+  | $nl        { ascend $ mkT TSpace }
<paren_sc'>                 @newpar                         { ascend $ mkT TNewPar }
<paren_sc'>                 $typing / [[^\[\(] $nl]         { ascend $ mkTLetter }
<paren_sc'>                 \\ @varid                       { ascend $ mkTBS (TVarId . C8.tail) }
<paren_sc'>                 @varid / \(                     { mkTBS TVarId }
<paren_sc'>                 @varid / \[                     { mkTBS TVarId }
<paren_sc'>                 \]                              { ascend (ascend (mkTBS TSpecial)) }

<paren_sc>                  \)                              { ascend (mkTBS TSpecial) }
<typing_sc>                 \]                              { ascend (mkTBS TSpecial) }

<paren_sc, struct_sc>       \{                              { descend struct_sc (mkTBS TSpecial) }
<struct_sc>                 \}                              { ascend (mkTBS TSpecial) }
<paren_sc, struct_sc>       $nl                             { skip }

<math_sc>                   $nl                             { skip }
<math_sc>                   @mathop                         { mkTBS TReservedOp }
<math_sc>                   $horizontalwhite+               { skip }
<math_sc>                   $math / [[^\[\(] $nl]           { mkTLetter }
<math_sc>                   \]                              { ascend (mkTBS TSpecial) }
<math_sc>                   [\{\}]                          { mkTBS TSpecial }

<functop_sc>                @varid                          { mkTBS TVarId }
<functop_sc>                [\(\[\)\]\,]                    { mkTBS TSpecial }
<functop_sc>                \{                              { ascend (descend script_sc (mkTBS TSpecial)) }
<scripttop_sc>              \(                              { descend paren_sc (mkTBS TSpecial) }
<scripttop_sc>              \{                              { ascend (descend script_sc (mkTBS TSpecial)) }
<functop_sc, scripttop_sc>  [$horizontalwhite $nl]+         { skip }

{

showSC :: SC -> String
showSC sc | sc == typing_sc = "typing_sc"
          | sc == math_sc = "math_sc"
          | sc == script_sc = "script_sc"
          | sc == comment_sc = "comment_sc"
          | sc == paren_sc = "paren_sc"
          | sc == paren_sc' = "paren_sc'"
          | sc == struct_sc = "struct_sc"
          | sc == functop_sc = "functop_sc"
showSC _ = ""

data Token
  = TInteger !Int
  | TFloat !Double
  | TBool !Bool
  | TString BS.ByteString
  | TSpecial BS.ByteString
  | TNewLine
  | TReservedId BS.ByteString
  | TVarId BS.ByteString
  | TReservedOp BS.ByteString
  | TNewPar
  | TSpace
  | TLetter !Char
  | TEOF
    deriving (Show, Eq)

unToken :: Token -> String
unToken (TInteger i) = show i
unToken (TFloat f) = show f
unToken (TBool b) = if b then "true" else "false"
unToken (TString s) = show s
unToken (TSpecial s) = C8.unpack s
unToken TNewLine = "\n"
unToken (TReservedId s) = C8.unpack s
unToken (TVarId s) = C8.unpack s
unToken (TReservedOp o) = C8.unpack o
unToken TNewPar = "\n\n"
unToken TSpace = " "
unToken (TLetter c) = [c]
unToken TEOF = ""

type LexerAction = SourceFeed -> Int64 -> LP (Positioned Token)

mkTFloat :: LexerAction
mkTFloat feed len = do
    let bs = C8.take len (feedData feed)
    case BSR.fractional bs of
        Nothing -> lpErr "error reading integer literal"
        Just (i,_) -> return $ Pos (feedPosition feed) (TFloat i)

mkTInteger :: LexerAction
mkTInteger feed len = do
    let bs = C8.take len (feedData feed)
    case BSR.integral bs of
        Nothing -> lpErr "error reading integer literal"
        Just (i,_) -> return $ Pos (feedPosition feed) (TInteger i)

mkTBool :: LexerAction
mkTBool feed len = do
    let bs = C8.take len (feedData feed)
    return . Pos (feedPosition feed) . TBool $ case bs of
        "true" -> True
        "false" -> False

mkTBS :: (BS.ByteString -> Token) -> LexerAction
mkTBS t feed len = return $ Pos (feedPosition feed) (t $ C8.take len (feedData feed))

mkT :: Token -> LexerAction
mkT t feed _ = return $ Pos (feedPosition feed) t

mkTLetter :: LexerAction
mkTLetter feed len = return $ Pos (feedPosition feed) (TLetter $ C8.head (feedData feed))

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

scan :: LP (Positioned Token)
scan = do
    feed <- gets sourceFeed
    sc <- gets currentSC
    case alexScan feed sc of
        AlexEOF -> return $ Pos (feedPosition feed) TEOF
        AlexError feed' -> lpErr "lexical error"
        AlexSkip feed' len -> do
            modify' $ \st -> st { sourceFeed = feed' }
            scan
        AlexToken feed' len action -> do
            modify' $ \st -> st { sourceFeed = feed' }
            action feed (fromIntegral len)

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

alex_tab_size = 4

alexMove :: SourcePosition -> Char -> SourcePosition
alexMove (SourcePosition a l c) '\t' = SourcePosition (a+1)  l     (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (SourcePosition a l c) '\n' = SourcePosition (a+1) (l+1)   1
alexMove (SourcePosition a l c) _    = SourcePosition (a+1)  l     (c+1)

}
