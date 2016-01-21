module Main where

import qualified Data.ByteString.Lazy as BS

import Kai.Parse
import Kai.LP
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Text.PrettyPrint.Leijen as PP

main :: IO ()
main = prettyParse =<< BS.readFile "/home/joshkirklin/kai-test.kai"

prettyParse :: BS.ByteString -> IO ()
prettyParse d = do
    let ef = parse d
    case ef of
        Left e -> PP.putDoc (prettyCompileMsg e d)
        Right f -> PP.putDoc (PP.pretty f)
    putStrLn ""
