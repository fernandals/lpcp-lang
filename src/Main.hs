module Main where

import Lexer
import Parser
import System.Environment
import System.IO.Unsafe

main :: IO ()
main = do
  (file : _) <- getArgs
  parsed <- parser $ getTokens file
  case parsed of
    Left e -> print e
    Right ans -> print () -- print ans
