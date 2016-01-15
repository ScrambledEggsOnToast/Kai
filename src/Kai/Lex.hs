module Kai.Lex where

import qualified Kai.Lex.Internal as I (scanAll, scan, typing, math, lua)

typing = I.typing
math = I.math
lua = I.lua
scan = I.scan
scanAll = I.scanAll
