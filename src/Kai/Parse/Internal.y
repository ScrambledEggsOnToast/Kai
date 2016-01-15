{
{-# LANGUAGE OverloadedStrings, GADTs #-}
module Kai.Parse.Internal where

import Kai.LP
import {-# SOURCE #-} Kai.Lex (scan)

import Data.Loc

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..), (<|))

import qualified Language.Lua.Token as Lua
import Language.Lua.Syntax

import qualified Data.ByteString.Lazy.Char8 as C8

import Data.Monoid ((<>))

import GHC.Exts (toList)

}

%name parseBlock Block
%name parseStatement Stat
%name parseCall Funccall
%tokentype { Token }
%error { parseError }

%monad { LP }
%lexer { ((unLoc <$> scan) >>=) } { TkEOF }

%token
    and         { TkLua Lua.TkAnd }
    break       { TkLua Lua.TkBreak }
    do          { TkLua Lua.TkDo }
    else        { TkLua Lua.TkElse }
    elseif      { TkLua Lua.TkElseif }
    end         { TkLua Lua.TkEnd }
    false       { TkLua Lua.TkFalse }
    for         { TkLua Lua.TkFor }
    function    { TkLua Lua.TkFunction }
    goto        { TkLua Lua.TkGoto }
    if          { TkLua Lua.TkIf }
    in          { TkLua Lua.TkIn }
    local       { TkLua Lua.TkLocal }
    nil         { TkLua Lua.TkNil }
    not         { TkLua Lua.TkNot }
    or          { TkLua Lua.TkOr }
    repeat      { TkLua Lua.TkRepeat }
    return      { TkLua Lua.TkReturn }
    then        { TkLua Lua.TkThen }
    true        { TkLua Lua.TkTrue }
    until       { TkLua Lua.TkUntil }
    while       { TkLua Lua.TkWhile }
    '+'         { TkLua Lua.TkPlus }
    '-'         { TkLua Lua.TkDash }
    '*'         { TkLua Lua.TkMult }
    '/'         { TkLua Lua.TkFloatDiv }
    '%'         { TkLua Lua.TkModulo }
    '^'         { TkLua Lua.TkExponent }
    '#'         { TkLua Lua.TkLength }
    '&'         { TkLua Lua.TkBitwiseAnd }
    '~'         { TkLua Lua.TkTilde }
    '|'         { TkLua Lua.TkBitwiseOr }
    '<<'        { TkLua Lua.TkLShift }
    '>>'        { TkLua Lua.TkRShift }
    '//'        { TkLua Lua.TkFloorDiv }
    '=='        { TkLua Lua.TkEq }
    '~='        { TkLua Lua.TkNeq }
    '<='        { TkLua Lua.TkLeq }
    '>='        { TkLua Lua.TkGeq }
    '<'         { TkLua Lua.TkLt }
    '>'         { TkLua Lua.TkGt }
    '='         { TkLua Lua.TkAssign }
    '('         { TkLua Lua.TkLParen }
    ')'         { TkLua Lua.TkRParen }
    '{'         { TkLua Lua.TkLBrace }
    '}'         { TkLua Lua.TkRBrace }
    '['         { TkLua Lua.TkLBracket }
    ']'         { TkLua Lua.TkRBracket }
    '::'        { TkLua Lua.TkLabel }
    ';'         { TkLua Lua.TkSemi }
    ':'         { TkLua Lua.TkColon }
    ','         { TkLua Lua.TkComma }
    '.'         { TkLua Lua.TkDot }
    '..'        { TkLua Lua.TkConcat }
    '...'       { TkLua Lua.TkVararg }
    '\''        { TkLua Lua.TkQuote }
    '"'         { TkLua Lua.TkDoubleQuote }
    ident       { TkLua (Lua.TkIdent $$) }
    string      { TkLua (Lua.TkStringLit $$) }
    int         { TkLua (Lua.TkIntLit $$) }
    float       { TkLua (Lua.TkFloatLit $$) }

%left or
%left and
%nonassoc '<' '>' '<=' '>=' '~=' '=='
%left '|'
%left '&'
%left '<<' '>>'
%right '..'
%left '+' '-'
%left '*' '/' '//' '%'
%right '^'

%%

Block           : Retstat                                           { Block () [] (Just $1) }    
                | {- empty -}                                       { Block () [] Nothing }
                | Stat Block                                        { $2 `blockAdd` $1 }

Stat            : ';'                                               { EmptyStmt () }
                | Varlist1 '=' Explist1                             { Assign () $1 $3 }
                | Funccall                                          { FunCall () $1 }
                | Label                                             { Label () $1 }
                | break                                             { Break () }
                | goto Ident                                        { Goto () $2 }
                | do Block end                                      { Do () $2 }
                | while Exp do Block end                            { While () $2 $4 }
                | repeat Block until Exp                            { Repeat () $2 $4 }
                | If                                                { $1 }
                | for Ident '=' Exp ',' Exp do Block end            { For () $2 $4 $6 Nothing $8 }
                | for Ident '=' Exp ',' Exp ',' Exp do Block end    { For () $2 $4 $6 (Just $8) $10 }
                | for Identlist1 in Explist1 do Block end           { ForIn () $2 $4 $6 }
                | function Funcname Funcbody                        { FunAssign () $2 $3 }
                | local function Ident Funcbody                     { LocalFunAssign () $3 $4 }
                | local Identlist1                                  { LocalAssign () $2 (ExpressionList () []) }
                | local Identlist1 '=' Explist                      { LocalAssign () $2 $4 }

If              : If1 end                                           { $1 }
                | If1 else Block end                                { $1 `withElse` $3 }
If1             : if Exp then Block ElseIf                          { $5 `ifAdd` ($2,$4) }
                | if Exp then Block                                 { If () (pure ($2,$4)) Nothing }
ElseIf          : ElseIf1                                           { If () (pure $1) Nothing }
                | ElseIf1 ElseIf                                    { $2 `ifAdd` $1 }
ElseIf1         : elseif Exp then Block                             { ($2,$4) }

Retstat         : return Explist                                    { ReturnStatement () $2 }
                | return Explist ';'                                { ReturnStatement () $2 }

Label           : '::' Ident '::'                                   { $2 }

Funcname        : Funcname1                                         { FunctionName () (IdentList1 () $1) Nothing }
                | Funcname1 ':' Ident                               { FunctionName () (IdentList1 () $1) (Just $3) }
Funcname1       : Ident                                             { pure $1 }
                | Ident '.' Funcname1                               { $1 <| $3 }

Varlist1        : Varlist11                                         { VariableList1 () $1 }
Varlist11       : Var                                               { pure $1 }
                | Var ',' Varlist11                                 { $1 <| $3 }
  
Var             : Ident                                             { VarIdent () $1 }
                | Prefixexp '[' Exp ']'                             { VarField () $1 $3 }
                | Prefixexp '.' Ident                               { VarFieldName () $1 $3 }

Identlist1      : Identlist11                                       { IdentList1 () $1 }
Identlist11     : Ident                                             { pure $1 }
                | Ident ',' Identlist11                             { $1 <| $3 }

Ident           : ident                                             { Ident () $1 }

Explist         : Explist11                                         { ExpressionList () (toList $1) }
                | {- empty -}                                       { ExpressionList () [] }

Explist1        : Explist11                                         { ExpressionList1 () $1 }
Explist11       : Exp                                               { pure $1 }
                | Exp ',' Explist11                                 { $1 <| $3 }

Exp             : nil                                               { Nil () }
                | false                                             { Bool () False }
                | true                                              { Bool () True }
                | int                                               { Integer () $1 }
                | float                                             { Float () $1 }
                | String                                            { String () $1 }
                | '...'                                             { Vararg () }
                | function Funcbody                                 { FunDef () $2 }
                | Prefixexp                                         { PrefixExp () $1 }
                | Tableconstructor                                  { TableCtor () $1 }
                | Exp Binop Exp                                     { Binop () $2 $1 $3 }
                | Unop Exp                                          { Unop () $1 $2 }

Prefixexp       : Var                                               { PrefixVar () $1 }
                | Funccall                                          { PrefixFunCall () $1 }
                | '(' Exp ')'                                       { Parens () $2 }

Funccall        : Prefixexp Args                                    { FunctionCall () $1 $2 }
                | Prefixexp ':' Ident Args                          { MethodCall () $1 $3 $4 }

Args            : '(' Explist ')'                                   { Args () $2 }
                | Tableconstructor                                  { ArgsTable () $1 }
                | String                                            { ArgsString () $1 }

Funcbody        : '(' ')' Block end                                 { FunctionBody () (IdentList () []) False $3 }
                | '(' '...' ')' Block end                           { FunctionBody () (IdentList () []) True $4 }
                | '(' Identlist11 ')' Block end                     { FunctionBody () (IdentList () (toList $2)) False $4 }
                | '(' Identlist11 ',' '...' ')' Block end           { FunctionBody () (IdentList () (toList $2)) True $6 }

Tableconstructor : '{' '}'                                          { TableConstructor () (FieldList () []) }
                | '{' Fieldlist '}'                                 { TableConstructor () (FieldList () $2) }
                | '{' Fieldlist ',' '}'                             { TableConstructor () (FieldList () $2) }
                | '{' Fieldlist ';' '}'                             { TableConstructor () (FieldList () $2) }

Fieldlist       : Field                                             { [$1] }
                | Field ',' Fieldlist                               { $1:$3 }
                | Field ';' Fieldlist                               { $1:$3 }

Field           : '[' Exp ']' '=' Exp                               { FieldExp () $2 $5 }
                | Ident '=' Exp                                     { FieldIdent () $1 $3 }
                | Exp                                               { Field () $1 }

Binop           : '+'                                               { Plus () }
                | '-'                                               { Minus () }
                | '*'                                               { Mult () }
                | '/'                                               { FloatDiv () }
                | '//'                                              { FloorDiv () }
                | '^'                                               { Exponent () }
                | '%'                                               { Modulo () }
                | '&'                                               { BitwiseAnd () }
                | '~'                                               { BitwiseXor () }
                | '|'                                               { BitwiseOr () }
                | '>>'                                              { Rshift () }
                | '<<'                                              { Lshift () }
                | '..'                                              { Concat () }
                | '<'                                               { Lt () }
                | '<='                                              { Leq () }
                | '>'                                               { Gt () }
                | '>='                                              { Geq () }
                | '=='                                              { Eq () }
                | '~='                                              { Neq () }
                | and                                               { And () }
                | or                                                { Or () }

Unop            : '-'                                               { Negate () }
                | not                                               { Not () }
                | '#'                                               { Length () }
                | '~'                                               { BitwiseNot () }

String          : '\'' string '\''                                  { $2 }
                | '"' string '"'                                    { $2 }

{

blockAdd (Block x ss mr) s = Block x (s:ss) mr
withElse (If x is me) e = If x is (Just e)
ifAdd (If x is me) (i,e) = If x ((i,e)<|is) me

data Typing = Typing

parseError t = lpErr $ "parse error on " <> C8.pack (show t)

}
