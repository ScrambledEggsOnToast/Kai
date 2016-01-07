{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses, FlexibleInstances #-}
module Kai.AST where

import GHC.Generics
import Data.Typeable
import Data.Data
import Control.Lens (Traversal', ignored)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Monoid ((<>))

import Unbound.Generics.LocallyNameless

-- Types

newtype Typing = T [Paragraph]
    deriving (Show, Generic, Typeable)

type Paragraph = [Character]

data Character
    = Script Script
    | Math Math
    | Letter Char
    | Space
    | TypingCall Ident [Exp] [Typing]
    deriving (Show, Generic, Typeable)

type Math = [MathExp]

data MathExp
    = Symbol Char
    | Sub Math Math
    | Super Math Math
    | MathCall Ident [Exp] [Typing]
    deriving (Show, Generic, Typeable)

type Script = [ScriptLn]

data ScriptLn
    = Exp Exp
    | Assign Ident Exp 
    | PlusAssign Ident Exp 
    | MinusAssign Ident Exp 
    | MultAssign Ident Exp 
    | DivAssign Ident Exp 
    | FunDec Ident (Bind ([Name Exp], [Name Typing]) Script)
    | ForIn Exp (Bind (Name Exp) Script)
    | IfElse Exp Script Script
    | LoadClass Ident
    | ImportPackage Ident
    | Print Exp
    | Return Exp
    | Break
    | Continue
    deriving (Show, Generic, Typeable)

data Exp
    = Oper Op Exp Exp
    | Neg Exp
    | Lit Lit
    | Typing Typing
    | List [Exp]
    | Empty
    | Struct [(Ident, Exp)]
    | Var (Name Exp)
    | Call Ident [Exp] [Typing]
    deriving (Show, Generic, Typeable)

data Op = Plus | Minus | Mult | Div | Ass | Eq | Less | More | LessEq | MoreEq | Pow | Range
    deriving (Show, Generic, Typeable, Data)

data Lit
    = Int Int
    | Float Double
    | Bool Bool
    | String Text
    deriving (Show, Generic, Typeable, Data)

newtype Text = Text { unText :: BS.ByteString }
    deriving (Show, Generic, Typeable, Data)

type Ident = Text

-- Find class and instances
traverse' f [e] = f e
traverse' f (e:es) = f e *> traverse' f es

class Children s a where
    children :: Traversal' s a
    children = ignored

instance Children s a => Children [s] a where
    children _ [] = pure []
    children f ss = traverse' (children f) ss *> pure ss
      where
        traverse' g [e] = g e
        traverse' g (e:es) = g e *> traverse' g es

instance Children Character Exp where
    children f s = case s of
        s@(TypingCall _ es@(_:_) ts) -> traverse' f es 
                                     *> pure s
        e -> pure e
instance Children Exp Exp where
    children f s = case s of
        Oper _ e1 e2    -> f e1 *> f e2                 *> pure s
        Neg e           -> f e                          *> pure s
        List es         -> (children f) es              *> pure s
        Struct ies      -> (children f) (map snd ies)   *> pure s
        Call _ es _     -> (children f) es              *> pure s
        e -> pure e

-- Alpha instances

instance Alpha Character
instance Alpha Typing
instance Alpha Lit
instance Alpha Op
instance Alpha MathExp
instance Alpha ScriptLn
instance Alpha Exp
instance Alpha Text where
    aeq' _ctx (Text i) (Text j) = i == j

    fvAny' _ctx _nfn i = pure i

    close _ctx _b i = i
    open _ctx _b i = i

    isPat _ = mempty
    isTerm _ = mempty

    nthPatFind _ = mempty
    namePatFind _ = mempty

    swaps' _ctx _p i = i
    freshen' _ctx i = return (i, mempty)
    lfreshen' _ctx i cont = cont i mempty

    acompare' _ctx (Text i) (Text j) = compare i j


-- Subst instances

instance Subst Exp Exp where
    isvar (Var e) = Just (SubstName e)
    isvar _ = Nothing
instance Subst Exp Typing
instance Subst Exp Lit
instance Subst Exp Character
instance Subst Exp ScriptLn
instance Subst Exp MathExp
instance Subst Exp Op
instance Subst Exp Text where
    subst _ _ x = x
    substs _ x = x

-- Helpers

var :: Ident -> Exp
var = Var . ident2name

fundec :: Ident -> [Ident] -> [Ident] -> Script -> ScriptLn
fundec i es ts s = FunDec i $ bind (map ident2name es, map ident2name ts) s

forin :: Ident -> Exp -> Script -> ScriptLn
forin i e s = ForIn e $ bind (ident2name i) s

ident2name = string2Name . C8.unpack . unText
