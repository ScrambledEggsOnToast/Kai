{-# LANGUAGE OverloadedStrings #-}
module Kai.Syntax where

import qualified Language.Lua.Syntax as Lua
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Sequence ((|>),(<|), ViewL(..), ViewR(..))
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Monoid
import qualified Data.Foldable as F

import qualified Text.PrettyPrint.Leijen as PP

data KaiLua a = Call !a (Lua.FunctionCall ()) | Block !a (Lua.Block a)
    deriving (Show, Eq)
newtype LuaRef = LuaRef Int
    deriving (Show, Eq)

data TypingBit a
    = TypingLua !a LuaRef -- 'bound' to a (KaiLua a)
    | TypingNewPar !a
    | TypingSpace !a
    | TypingSymbol !a BS.ByteString
    | TypingMath !a (Math a)
    deriving (Show, Eq)

instance PP.Pretty (TypingBit a) where
    pretty (TypingLua _ (LuaRef r)) = PP.text $ "<" <> show r <> ">"
    pretty (TypingNewPar _) = PP.line PP.<> PP.line
    pretty (TypingSpace _) = PP.text " "
    pretty (TypingSymbol _ bs) = PP.text (C8.unpack bs)
    pretty (TypingMath _ m) = PP.hcat [PP.text "[", PP.pretty m, PP.text "]"]

data Typing a = Typing !a (S.Seq (TypingBit a))
    deriving (Show, Eq)

instance PP.Pretty (Typing a) where
    pretty (Typing _ tbs) = PP.hcat . map PP.pretty . F.toList $ tbs

data MathBit a
    = MathLua !a LuaRef
    | MathOp !a MathOp (MathBit a) (MathBit a)
    | MathSymbol !a BS.ByteString
    | SubMath !a (Math a)
    deriving (Show, Eq)

instance PP.Pretty (MathBit a) where
    pretty (MathLua _ (LuaRef r)) = PP.text $ "<" <> show r <> ">"
    pretty (MathOp _ op x y) = PP.hsep [PP.pretty x, PP.pretty op, PP.pretty y]
    pretty (MathSymbol _ bs) = PP.text . C8.unpack $ bs
    pretty (SubMath _ m) = PP.hcat [PP.text "{", PP.pretty m, PP.text "}"]

data MathOp = MathSub | MathSup
    deriving (Show, Eq)

instance PP.Pretty MathOp where
    pretty MathSub = PP.text "_"
    pretty MathSup = PP.text "^"

data Math a = Math !a (S.Seq (MathBit a))
    deriving (Show, Eq)

instance PP.Pretty (Math a) where
    pretty (Math _ mbs) = PP.hsep . map PP.pretty . F.toList $ mbs

data KaiDoc a = KaiDoc
    { kaiDocAnnotation :: a
    , kaiRoot :: Lua.FunctionCall ()
    , kaiDocTypings :: V.Vector (Typing a)
    } 
    deriving (Show, Eq)

instance PP.Pretty (KaiDoc a) where
    pretty kd = PP.vsep $ PP.pretty (kaiRoot kd) : (concatMap (\(n,t) -> [PP.text $ "--- Typing " <> show n <> " ---", PP.pretty t]) . zip [0..] . F.toList . kaiDocTypings $ kd)
