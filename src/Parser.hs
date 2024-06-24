{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import Builtin
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IntMap (update)
import Declarations
import Errors
import Expressions
import ExpressionsParser
import FluxControl
import Functions
import GHC.IO.FD (stdout)
import Lexer
import State
import Statements
import System.IO
import Text.Parsec hiding (State)
import Text.Printf (printf)
import Text.Read
import Tokens
import Utils

parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program defaultState "Error"

program :: ParsecT [Token] State IO [Token]
program = do
  p <- moduleToken <?> missingModuleErrorMsg
  pn <- idToken <?> missingModuleErrorMsg
  d <- many (try funDecl <|> varDecl)
  main <- mainProgram <?> missingMainErrorMsg
  eof
  return $ [p, pn] ++ concat d ++ main

mainProgram :: ParsecT [Token] State IO [Token]
mainProgram = do
  fun <- funToken
  main <- mainFun
  beginpToken >> endpToken

  let scope_name = scopeNameBlock "_global_" "main"

  updateState $ setFlag True
  updateState $ pushStack scope_name

  block <- blockParser

  return $ main : block
