module Main where

import Lexer
import Parser
import System.Environment
import System.IO.Unsafe

main :: IO ()
main = do
  (file : _) <- getArgs
  case parser $ getTokens file of
    Left e -> print e
    Right ans -> print ans
