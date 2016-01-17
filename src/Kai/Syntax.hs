module Kai.Syntax where

import qualified Language.Lua.Syntax as Lua
import qualified Data.ByteString.Lazy as BS
import Data.Sequence ((|>),(<|), ViewL(..), ViewR(..))
import qualified Data.Sequence as S
import qualified Data.Vector as V

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

data Typing a = Typing !a (S.Seq (TypingBit a))
    deriving (Show, Eq)

data MathBit a
    = MathLua !a LuaRef
    | MathOp !a MathOp (MathBit a) (MathBit a)
    | MathSymbol !a BS.ByteString
    | SubMath !a (Math a)
    deriving (Show, Eq)

data MathOp = MathSub | MathSup
    deriving (Show, Eq)

data Math a = Math !a (S.Seq (MathBit a))
    deriving (Show, Eq)

data KaiDoc a = KaiDoc
    { kaiDocAnnotation :: a
    , kaiRoot :: Lua.FunctionCall ()
    , kaiDocTypings :: V.Vector (Typing a)
    } 
    deriving (Show, Eq)
