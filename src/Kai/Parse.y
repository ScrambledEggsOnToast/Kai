{
{-# LANGUAGE OverloadedStrings #-}
module Kai.Parse where

import Kai.LP
import Kai.Lex
import Kai.AST

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
    ident1      { TVarId $$ }
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

ident               : ident1                                        { Text $1 }

Typing              : Typing1                                       { T $1 }
Typing1             : Typing1 newpar Paragraph                      { $3 : $1 }
                    | Typing1 newpar                                { $1 }
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
                    | for ident in '(' Exp ')' '{' Script '}'       { forin $2 $5 $8 }
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

FunDecTop           : function ident TopParams TopTypings           { fundec $2 $3 $4 }
TopParams           : {- empty -}                                   { [] }
                    | '(' TopParams1 ')'                            { $2 }
                    | '(' ')'                                       { [] }
TopParams1          : ident                                         { [$1] }
                    | TopParams1 ',' ident                          { $3 : $1 }
TopTypings          : {- empty -}                                   { [] }
                    | TopTypings '[' ident ']'                      { $3 : $1 }

Lit                 : intLit                                        { Int $1 }
                    | floatLit                                      { Float $1 }
                    | boolLit                                       { Bool $1 }
                    | stringLit                                     { String (Text $1) }

Exp                 : '(' Exp ')'                                   { $2 }
                    | Exp '+' Exp                                   { Oper Plus $1 $3 }
                    | Exp '-' Exp                                   { Oper Minus $1 $3 }
                    | Exp '*' Exp                                   { Oper Mult $1 $3 }
                    | Exp '/' Exp                                   { Oper Div $1 $3 }
                    | Exp '==' Exp                                  { Oper Eq $1 $3 }
                    | Exp '<' Exp                                   { Oper Less $1 $3 }
                    | Exp '>' Exp                                   { Oper More $1 $3 }
                    | Exp '<=' Exp                                  { Oper LessEq $1 $3 }
                    | Exp '>=' Exp                                  { Oper MoreEq $1 $3 }
                    | Exp '^' Exp                                   { Oper Pow $1 $3 }
                    | Exp '..' Exp                                  { Oper Range $1 $3 }
                    | Lit                                           { Lit $1 }
                    | '[' Typing ']'                                { Typing $2 }
                    | '{' '}'                                       { Empty }
                    | List                                          { List $1 }
                    | Struct                                        { Struct $1 }
                    | ident CallParams CallTypings                  { Call $1 $2 $3 }
                    | ident CallParams                              { Call $1 $2 [] }
                    | ident CallTypings                             { Call $1 [] $2 }
                    | '\\' ident                                    { Call $2 [] [] }
                    | ident                                         { var $1 }

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

}
