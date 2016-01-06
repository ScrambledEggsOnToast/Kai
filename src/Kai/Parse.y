{
{-# LANGUAGE OverloadedStrings #-}
module Kai.Parse where

import Kai.LP
import Kai.Lex

import qualified Data.ByteString.Lazy as BS
}

%name parseLP
%tokentype { Token }
%error { parseError }

%monad { LP }
%lexer { ((fmap stripPosition scan) >>=) } { TEOF }

%token
    intLit      { TInteger $$ }
    floatLit    { TFloat $$ }
    boolLit     { TBool $$ }
    stringLit   { TString $$ }
    ','         { TSpecial "," }
    ':'         { TSpecial ":" }
    '('         { TSpecial "(" }
    ')'         { TSpecial ")" }
    '['         { TSpecial "[" }
    ']'         { TSpecial "]" }
    '{'         { TSpecial "{" }
    '}'         { TSpecial "}" }
    nl          { TNewLine }
    class       { TReservedId "class" }
    import      { TReservedId "import" }
    function    { TReservedId "function" }
    return      { TReservedId "return" }
    for         { TReservedId "for" }
    in          { TReservedId "in" }
    if          { TReservedId "if" }
    then        { TReservedId "then" }
    else        { TReservedId "else" }
    break       { TReservedId "break" }
    continue    { TReservedId "continue" }
    true        { TReservedId "true" }
    false       { TReservedId "false" }
    print       { TReservedId "import" }
    ident       { TVarId $$ }
    '+'         { TReservedOp "+" }
    '-'         { TReservedOp "-" }
    '*'         { TReservedOp "*" }
    '/'         { TReservedOp "/" }
    '='         { TReservedOp "=" }
    '+='        { TReservedOp "+=" }
    '-='        { TReservedOp "-=" }
    '*='        { TReservedOp "*=" }
    '/='        { TReservedOp "/=" }
    '=='        { TReservedOp "==" }
    '^'         { TReservedOp "^" }
    '_'         { TReservedOp "_" }
    '..'        { TReservedOp ".." }
    '...'       { TReservedOp "..." }
    par         { TNewPar }
    ' '         { TSpace }
    letter      { TLetter $$ }

%%

Exp     : Exp '+' Term          { Plus $1 $3 }
        | Exp '-' Term          { Minus $1 $3 }
        | Term                  { Term $1 }

Term    : Term '*' Factor       { Mult $1 $3 }
        | Term '/' Factor       { Div $1 $3 }
        | Factor                { Factor $1 }

Factor  : intLit                { Int $1 }
        | floatLit              { Float $1 }
        | boolLit               { Bool $1 }
        | stringLit             { String $1 }
        | ident                 { Var $1 }
        | '(' Exp ')'           { Brack $2 }

{

parse :: BS.ByteString -> Either CompileMsg Exp
parse = runLP parseLP script_sc

parseError _ = lpErr "parse error"

data Exp
    = Plus Exp Term
    | Minus Exp Term
    | Term Term
    deriving Show

data Term
    = Mult Term Factor
    | Div Term Factor
    | Factor Factor
    deriving Show

data Factor
    = Int Int
    | Float Double
    | Bool Bool
    | String BS.ByteString
    | Var BS.ByteString
    | Brack Exp
    deriving Show

}
