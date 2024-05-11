module Main where

import Lex
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      input <- readFile filename
      let tokens = alexScanTokens input
      print tokens
    _ -> putStrLn "Usage: file"
