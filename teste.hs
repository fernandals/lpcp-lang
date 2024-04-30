module Main where

import System.Environment
import Lex

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            input <- readFile filename
            let tokens = alexScanTokens input
            print tokens
        _ -> putStrLn "Usage: programa arquivo"