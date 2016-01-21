module Main where

import qualified Data.ByteString.Lazy as BS

import Kai.Parse
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Text.PrettyPrint.Leijen as PP

main :: IO ()
main = do
    Right f <- parse <$> BS.readFile "/home/joshkirklin/kai-test.kai"
    PP.putDoc (PP.pretty f)
    putStrLn ""
