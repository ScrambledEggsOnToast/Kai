module Kai.Parse (parse, parseCall) where

import Kai.LP
import Kai.Syntax
import qualified Kai.Parse.Internal as I (parseTyping, parseCall)
import Kai.Lex

import qualified Data.ByteString.Lazy as BS

import qualified Data.Vector as V
import qualified Data.Foldable as F

parseCall = I.parseCall

parse :: BS.ByteString -> Either CompileMsg (KaiDoc ())
parse s = do
    (x, ts) <- runLP I.parseTyping typing s
    return $ KaiDoc () x (V.fromList . F.toList $ ts)
