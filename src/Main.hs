-- Main parser

module Main where

import Lexer
import Parser
import System.Environment
import System.IO.Unsafe

-- | Parses any given code.
-- Any code that complies with the defined rules.
main :: IO ()
main = do
  (file : _) <- getArgs
  parsed <- parser $ getTokens file
  case parsed of
    Left e -> print e
    Right ans -> return ()
