module Main where

import qualified Data.ByteString.Lazy as BS

import Kai.LP
import Kai.Lex

main :: IO ()
main = do
    f <- BS.readFile "/home/joshkirklin/kai-test.kai"
    print $ runLP scanAll typing f
