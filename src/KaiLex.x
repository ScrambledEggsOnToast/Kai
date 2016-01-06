{
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs -funbox-strict-fields #-}
module KaiLex where

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
$special   = [\,\:]

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

@newpar = $nl ($horizontalwhite* $nl)+

@reservedid =
    class|import|function|return|for|in|if|then|else|break|continue|true|false|print

@scriptop =
    "+"|"-"|"*"|"/"|"="|"+="|"-="|"*="|"/="|"=="|"^"|".."

@mathop = 
    "^"|"_"|"..."

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

<typing_sc, script_sc>      \{                              { descend script_sc (mkTBS TSpecial) }
<typing_sc, script_sc>      \}                              { ascend (mkTBS TSpecial) }

<script_sc>                 @reservedid                     { mkTBS TReservedId }
<script_sc>                 $nl                             { mkT TNewLine }
<script_sc, struct_sc,
    parenP_sc, parenB_sc>   $special                        { mkTBS TSpecial }
<script_sc, struct_sc,
    parenP_sc, parenB_sc>   @varid                          { mkTBS TVarId }
<script_sc, struct_sc,
    parenP_sc, parenB_sc>   @scriptop                       { mkTBS TReservedOp }
<script_sc, struct_sc, 
    parenP_sc, parenB_sc>   @decimal                        { mkTInteger }
<script_sc, struct_sc, 
    parenP_sc, parenB_sc>   @decimal \. @decimal @exponent? 
                                | @decimal @exponent        { mkTFloat }
<script_sc, struct_sc,
    parenP_sc, parenB_sc>   \" @string* \"                  { mkTBS (TString . C8.pack . read . C8.unpack) }
<script_sc, struct_sc,
    parenP_sc, parenB_sc>   $horizontalwhite+               { skip }
<script_sc, struct_sc,
    parenP_sc, parenB_sc>   \(                              { descend parenP_sc (mkTBS TSpecial) }
<script_sc, struct_sc,
    parenP_sc, parenB_sc>   \[                              { descend typing_sc (mkTBS TSpecial) }

<typing_sc>                 $horizontalwhite+  
                                | $horizontalwhite* $nl 
                                $horizontalwhite*           { mkT TSpace }
<typing_sc>                 @newpar                         { mkT TNewPar }
<typing_sc>                 $typing / [[^\(\[] $nl]         { mkTLetter }
<typing_sc>                 \[                              { descend math_sc (mkTBS TSpecial) }

<typing_sc, math_sc>        \\ @varid                       { mkTBS (TVarId . C8.tail) }
<typing_sc, math_sc>        @varid / \(                     { descend paren_sc' (mkTBS TVarId) }
<typing_sc, math_sc>        @varid / \[                     { descend paren_sc' (mkTBS TVarId) }

<paren_sc'>                 \(                              { descend parenP_sc (mkTBS TSpecial) }
<paren_sc'>                 \[                              { descend typing_sc (mkTBS TSpecial) }
<paren_sc'>                 $horizontalwhite+  | $nl        { ascend $ mkT TSpace }
<paren_sc'>                 @newpar                         { ascend $ mkT TNewPar }
<paren_sc'>                 $typing / [[^\(\[] $nl]         { ascend $ mkTLetter }
<paren_sc'>                 \\ @varid                       { ascend $ mkTBS (TVarId . C8.tail) }
<paren_sc'>                 @varid / \(                     { mkTBS TVarId }
<paren_sc'>                 @varid / \[                     { mkTBS TVarId }
<paren_sc'>                 \]                              { ascend (ascend (mkTBS TSpecial)) }

<parenP_sc>                 \)                              { ascend (mkTBS TSpecial) }
<typing_sc>                 \]                              { ascend (mkTBS TSpecial) }

<parenP_sc, struct_sc>      \{                              { descend struct_sc (mkTBS TSpecial) }
<struct_sc>                 \}                              { ascend (mkTBS TSpecial) }
<parenP_sc, struct_sc>      $nl                             { skip }

<math_sc>                   $nl                             { mkT TNewLine }
<math_sc>                   @mathop                         { mkTBS TReservedOp }
<math_sc>                   $horizontalwhite+               { skip }
<math_sc>                   $math / [^\(\[]                 { mkTLetter }
<math_sc>                   \]                              { ascend (mkTBS TSpecial) }

{

showSC :: SC -> String
showSC sc | sc == typing_sc = "typing_sc"
          | sc == math_sc = "math_sc"
          | sc == script_sc = "script_sc"
          | sc == comment_sc = "comment_sc"
          | sc == parenP_sc = "parenP_sc"
          | sc == paren_sc' = "paren_sc'"
          | sc == struct_sc = "struct_sc"
showSC _ = ""

data CompileMsg = CompileMsg { msgPosition :: SourcePosition, msgDescription :: BS.ByteString }
    deriving Show
emptyCompileMsg = CompileMsg { msgPosition = startPosition, msgDescription = "" }

prettyCompileMsg :: CompileMsg -> String
prettyCompileMsg msg = (show (lineNumber $ msgPosition msg)) ++ ":" ++ (show (lineNumber $ msgPosition msg)) ++ ": " ++ C8.unpack (msgDescription msg)

data SourcePosition = SourcePosition { charOffset :: !Int64, lineNumber :: !Int64, columnNumber :: !Int64 }
    deriving (Show, Eq)
startPosition = SourcePosition 0 1 1

data LexerState = LexerState
  { lexerFeed :: LexerFeed
  , lexerSC :: !SC
  , lexerBreadcrumbs :: [SC]
  }
    deriving (Show, Eq)
initLexerState = LexerState
  { lexerFeed = emptyFeed
  , lexerSC = 0
  , lexerBreadcrumbs = []
  }

newtype Lexer a = Lexer { unLexer :: ExceptT CompileMsg (StateT LexerState IO) a }

instance Functor Lexer where
    fmap f (Lexer m) = Lexer (fmap f m)
instance Applicative Lexer where
    pure = Lexer . pure
    (Lexer f) <*> (Lexer x) = Lexer (f <*> x)
instance Monad Lexer where
    return = Lexer . return
    (Lexer m) >>= f = Lexer (m >>= unLexer . f)
instance MonadState LexerState Lexer where
    get = Lexer get
    put = Lexer . put
instance MonadError CompileMsg Lexer where
    throwError = Lexer . throwError
    catchError (Lexer m) f = Lexer (catchError m (unLexer . f))
instance MonadIO Lexer where
    liftIO = Lexer . liftIO

runLexer :: Lexer a -> SC -> BS.ByteString -> IO (Either CompileMsg a)
runLexer (Lexer e) sc inp = evalStateT (runExceptT e) (initLexerState { lexerFeed = emptyFeed { feedData = inp }, lexerSC = sc })

lexerErr :: BS.ByteString -> Lexer a
lexerErr d = do
    p <- gets (feedPosition . lexerFeed)
    throwError $ emptyCompileMsg { msgPosition = p, msgDescription = d }

data LexerFeed = LexerFeed
  { feedPosition :: !SourcePosition
  , feedPrevChar :: !Char
  , feedData :: BS.ByteString
  }
    deriving (Show, Eq)
emptyFeed = LexerFeed
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

data Token
  = TInteger Int
  | TFloat Double
  | TString BS.ByteString
  | TSpecial BS.ByteString
  | TNewLine
  | TReservedId BS.ByteString
  | TVarId BS.ByteString
  | TReservedOp BS.ByteString
  | TNewPar
  | TSpace
  | TLetter Char
  | TEOF
    deriving (Show, Eq)

unToken :: Token -> String
unToken (TInteger i) = show i ++ " "
unToken (TFloat f) = show f ++ " "
unToken (TString s) = show s ++ " "
unToken (TSpecial s) = C8.unpack s ++ " "
unToken TNewLine = "\n"
unToken (TReservedId s) = C8.unpack s ++ " "
unToken (TVarId s) = C8.unpack s ++ " "
unToken (TReservedOp o) = C8.unpack o ++ " "
unToken TNewPar = "\n\n"
unToken TSpace = " "
unToken (TLetter c) = [c]
unToken TEOF = ""

type LexerAction = LexerFeed -> Int64 -> Lexer (Positioned Token)

mkTFloat :: LexerAction
mkTFloat feed len = do
    let bs = C8.take len (feedData feed)
    case BSR.fractional bs of
        Nothing -> lexerErr "error reading integer literal"
        Just (i,_) -> return $ Pos (feedPosition feed) (TFloat i)

mkTInteger :: LexerAction
mkTInteger feed len = do
    let bs = C8.take len (feedData feed)
    case BSR.integral bs of
        Nothing -> lexerErr "error reading integer literal"
        Just (i,_) -> return $ Pos (feedPosition feed) (TInteger i)

mkTBS :: (BS.ByteString -> Token) -> LexerAction
mkTBS t feed len = return $ Pos (feedPosition feed) (t $ C8.take len (feedData feed))

mkT :: Token -> LexerAction
mkT t feed _ = return $ Pos (feedPosition feed) t

mkTLetter :: LexerAction
mkTLetter feed len = return $ Pos (feedPosition feed) (TLetter $ C8.head (feedData feed))

andBegin :: LexerAction -> SC -> LexerAction
(act `andBegin` sc) input len = do 
    modify' $ \st -> st { lexerSC = sc }
    act input len

descend :: SC -> LexerAction -> LexerAction
descend sc con input len = do
    sc' <- gets lexerSC
    modify' $ \st -> st 
        { lexerBreadcrumbs = sc' : lexerBreadcrumbs st
        , lexerSC = sc
        }
    con input len

ascend :: LexerAction -> LexerAction
ascend con input len = do
    bc <- gets lexerBreadcrumbs
    case bc of
        [] -> lexerErr "illegal end block character"
        sc:scs -> do
            modify' $ \st -> st
                { lexerBreadcrumbs = scs
                , lexerSC = sc
                }
            con input len

type SC = Int

scan :: Lexer (Positioned Token)
scan = do
    feed <- gets lexerFeed
    sc <- gets lexerSC
    case alexScan feed sc of
        AlexEOF -> return $ Pos (feedPosition feed) TEOF
        AlexError feed' -> lexerErr "lexical error"
        AlexSkip feed' len -> do
            modify' $ \st -> st { lexerFeed = feed' }
            scan
        AlexToken feed' len action -> do
            modify' $ \st -> st { lexerFeed = feed' }
            action feed (fromIntegral len)

printAllLex :: Lexer ()
printAllLex = do
    l <- scan
    case l of
        Pos _ TEOF -> return ()
        _ -> do
            sc <- gets lexerSC
            liftIO $ putStrLn (showSC sc ++ " | " ++ show (stripPosition l))
            printAllLex

printAllUnToken :: Lexer ()
printAllUnToken = do
    l <- scan
    case l of
        Pos _ TEOF -> return ()
        _ -> do
            sc <- gets lexerSC
            liftIO $ putStr (unToken (stripPosition l))
            printAllUnToken


skip _ _ = scan

-- Alex compatability

type AlexInput = LexerFeed
alexGetByte :: LexerFeed -> Maybe (Word8, LexerFeed)
alexGetByte feed 
    | BS.null (feedData feed) = Nothing
    | otherwise = let 
            b = BS.head (feedData feed)
            d = BS.tail (feedData feed)
            c = BS.w2c b
            p = alexMove (feedPosition feed) c
        in p `seq` d `seq` Just (b, LexerFeed
            { feedPosition = p
            , feedData = d
            , feedPrevChar = c
            })
alexInputPrevChar :: LexerFeed -> Char
alexInputPrevChar = feedPrevChar

alex_tab_size = 4

alexMove :: SourcePosition -> Char -> SourcePosition
alexMove (SourcePosition a l c) '\t' = SourcePosition (a+1)  l     (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (SourcePosition a l c) '\n' = SourcePosition (a+1) (l+1)   1
alexMove (SourcePosition a l c) _    = SourcePosition (a+1)  l     (c+1)

}
