{
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Kai.Parse where

import Kai.LP
import Kai.Lex

import Data.Typeable
import Data.Data
import Control.Lens.Plated

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Monoid ((<>))
}

%name parseLP Typing
%tokentype { Token }
%error { parseError }

%monad { LP }
%lexer { ((fmap stripPosition scan) >>=) } { TEOF }

%token
    intLit      { TInteger $$ }
    floatLit    { TFloat $$ }
    boolLit     { TBool $$ }
    stringLit   { TString $$ }
    '\\'        { TSpecial "\\" }
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
    else        { TReservedId "else" }
    elseif      { TReservedId "elseif" }
    break       { TReservedId "break" }
    continue    { TReservedId "continue" }
    print       { TReservedId "print" }
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
    '<'         { TReservedOp "<" }
    '>'         { TReservedOp ">" }
    '<='        { TReservedOp "<=" }
    '>='        { TReservedOp ">=" }
    '^'         { TReservedOp "^" }
    '_'         { TReservedOp "_" }
    '..'        { TReservedOp ".." }
    newpar      { TNewPar }
    space       { TSpace }
    letter      { TLetter $$ }

%left '+' '-'
%left '*' '/'
%left '^' '_'
%nonassoc '..'
%nonassoc '==' '<' '>' '<=' '>='
%nonassoc ':'
%left ','
%nonassoc '=' '+=' '-=' '*=' '/='

%%

Typing              : Typing newpar Paragraph                       { $3 : $1 }
                    | Typing newpar                                 { $1 }
                    | Paragraph                                     { [$1] }
                    | {- empty -}                                   { [] }

Paragraph           : Character                                     { [$1] }
                    | Paragraph Character                           { $2 : $1 }

Character           : '{' Script '}'                                { Script $2 }
                    | '[' Math ']'                                  { Math $2 }
                    | letter                                        { Letter $1 }
                    | space                                         { Space }
                    | ident CallParams CallTypings                  { TypingCall $1 $2 $3 }
                    | ident CallParams                              { TypingCall $1 $2 [] }
                    | ident CallTypings                             { TypingCall $1 [] $2 }
                    | '\\' ident                                    { TypingCall $2 [] [] }

Math                : MathExp                                       { [$1] }
                    | Math MathExp                                  { $2 : $1 }
Math1               : '{' Math '}'                                  { $2 }
                    | MathExp                                       { [$1] }

MathExp             : letter                                        { Symbol $1 }
                    | Math1 '_' Math1                               { Sub $1 $3 }
                    | Math1 '^' Math1                               { Super $1 $3 }
                    | ident CallParams CallTypings                  { MathCall $1 $2 $3 }
                    | ident CallParams                              { MathCall $1 $2 [] }
                    | ident CallTypings                             { MathCall $1 [] $2 }
                    | '\\' ident                                    { MathCall $2 [] [] }

Script              : Script nl ScriptLn                            { $3 : $1 }
                    | Script nl                                     { $1 }
                    | ScriptLn                                      { [$1] }
                    | {- empty -}                                   { [] }

ScriptLn            : Exp                                           { Exp $1 }
                    | ident '=' Exp                                 { Assign $1 $3 }
                    | ident '+=' Exp                                { PlusAssign $1 $3 }
                    | ident '-=' Exp                                { MinusAssign $1 $3 }
                    | ident '*=' Exp                                { MultAssign $1 $3 }
                    | ident '/=' Exp                                { DivAssign $1 $3 }
                    | FunDecTop '{' Script '}'                      { $1 $3 }
                    | for ident in '(' Exp ')' '{' Script '}'       { ForIn $2 $5 $8 }
                    | If                                            { $1 }
                    | class '(' ident ')'                           { LoadClass $3 }
                    | import '(' ident ')'                          { ImportPackage $3 }
                    | print Exp                                     { Print $2 }
                    | return Exp                                    { Return $2 }
                    | break                                         { Break }
                    | continue                                      { Continue }

If                  : If1 else '{' Script '}'                       { $1 $4 }
                    | If1                                           { $1 [] }
If1                 : if '(' Exp ')' '{' Script '}'                 { IfElse $3 $6 }
                    | If1 elseif '(' Exp ')' '{' Script '}'         { \s -> $1 [IfElse $4 $7 s] }

FunDecTop           : function ident TopParams TopTypings           { FunDec $2 $3 $4 }
TopParams           : {- empty -}                                   { [] }
                    | '(' TopParams1 ')'                            { $2 }
                    | '(' ')'                                       { [] }
TopParams1          : ident                                         { [$1] }
                    | TopParams1 ',' ident                          { $3 : $1 }
TopTypings          : {- empty -}                                   { [] }
                    | TopTypings '[' ident ']'                      { $3 : $1 }

Exp                 : '(' Exp ')'                                   { $2 }
                    | Exp '+' Exp                                   { Plus $1 $3 }
                    | Exp '-' Exp                                   { Minus $1 $3 }
                    | Exp '*' Exp                                   { Mult $1 $3 }
                    | Exp '/' Exp                                   { Div $1 $3 }
                    | Exp '==' Exp                                  { Eq $1 $3 }
                    | Exp '<' Exp                                   { Less $1 $3 }
                    | Exp '>' Exp                                   { More $1 $3 }
                    | Exp '<=' Exp                                  { LessEq $1 $3 }
                    | Exp '>=' Exp                                  { MoreEq $1 $3 }
                    | Exp '^' Exp                                   { Pow $1 $3 }
                    | Exp '..' Exp                                  { Range $1 $3 }
                    | intLit                                        { Int $1 }
                    | floatLit                                      { Float $1 }
                    | boolLit                                       { Bool $1 }
                    | stringLit                                     { String $1 }
                    | '[' Typing ']'                                { Typing $2 }
                    | '{' '}'                                       { Empty }
                    | List                                          { List $1 }
                    | Struct                                        { Struct $1 }
                    | ident CallParams CallTypings                  { Call $1 $2 $3 }
                    | ident CallParams                              { Call $1 $2 [] }
                    | ident CallTypings                             { Call $1 [] $2 }
                    | '\\' ident                                    { Call $2 [] [] }
                    | ident                                         { Var $1 }

CallParams          : '(' CallParams1 ')'                           { $2 }
                    | '(' ')'                                       { [] }
CallParams1         : Exp                                           { [$1] }
                    | CallParams1 ',' Exp                           { $3 : $1 }

CallTypings         : '[' Typing ']'                                { [$2] }
                    | CallTypings '[' Typing ']'                    { $3 : $1 }

List                : '{' List1 '}'                                 { $2 }
List1               : Exp                                           { [$1] }
                    | List1 ',' Exp                                 { $3 : $1 }

Struct              : '{' Struct1 '}'                               { $2 }
Struct1             : StructEntry                                   { [$1] }
                    | Struct1 ',' StructEntry                       { $3 : $1 }
StructEntry         : ident ':' Exp                                 { ($1,$3) }
{

parse = runLP parseLP typing_sc

parseError t = lpErr $ "parse error at " <> (C8.pack $ show t)

type Ident = BS.ByteString

data Exp
    = Plus Exp Exp
    | Minus Exp Exp 
    | Mult Exp Exp
    | Div Exp Exp
    | Ass Exp Exp
    | Eq Exp Exp
    | Less Exp Exp
    | More Exp Exp
    | LessEq Exp Exp
    | MoreEq Exp Exp
    | Pow Exp Exp
    | Range Exp Exp
    | Neg Exp
    | Int Int
    | Float Double
    | Bool Bool
    | String BS.ByteString
    | Typing Typing
    | List [Exp]
    | Empty
    | Struct [(Ident, Exp)]
    | Var Ident
    | Call Ident [Exp] [Typing]
    deriving (Show, Eq, Data, Typeable)

instance Plated Exp

type Script = [ScriptLn]

data ScriptLn
    = Exp Exp
    | Assign Ident Exp 
    | PlusAssign Ident Exp 
    | MinusAssign Ident Exp 
    | MultAssign Ident Exp 
    | DivAssign Ident Exp 
    | FunDec Ident [Ident] [Ident] Script
    | ForIn Ident Exp Script
    | IfElse Exp Script Script
    | LoadClass Ident
    | ImportPackage Ident
    | Print Exp
    | Return Exp
    | Break
    | Continue
    deriving (Show, Eq, Data, Typeable)

type Typing = [Paragraph]

type Paragraph = [Character]

data Character
    = Script Script
    | Math Math
    | Letter Char
    | Space
    | TypingCall Ident [Exp] [Typing]
    deriving (Show, Eq, Data, Typeable)

type Math = [MathExp]

data MathExp
    = Symbol Char
    | Sub Math Math
    | Super Math Math
    | MathCall Ident [Exp] [Typing]
    deriving (Show, Eq, Data, Typeable)

}
