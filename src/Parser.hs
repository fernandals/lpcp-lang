{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import Builtin
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IntMap (update)
import Expressions
import Lexer
import State
import Text.Parsec hiding (State)
import Tokens
import Tokens (beginpToken)
import Utils

varDecl :: ParsecT [Token] State IO [Token]
varDecl = do
  modifier <- letToken <|> mutToken
  name <- idToken
  colon <- colonToken
  decltype <- types
  assign <- assignToken
  expr <- expression

  let expected_type = typeof decltype
  let actual_type = typeof expr

  when (actual_type == "error") $
    error $
      "Type mismatch in expression evaluation at "
        ++ show (pos name)
        ++ ".\n"
        ++ "Check the types of your operands.\n"
  when (expected_type /= actual_type) $
    error $
      "Type mismatch at "
        ++ show (pos name)
        ++ ".\n"
        ++ "Expected "
        ++ expected_type
        ++ ", got "
        ++ actual_type
        ++ ".\n"

  updateState $ stateInsert (modifier, decltype, name, expr)

  return
    [ modifier,
      name,
      colon,
      decltype,
      assign,
      expr
    ]

assign :: ParsecT [Token] State IO [Token]
assign = do
  name <- idToken
  assign <- assignToken
  expr <- expression

  updateState $ stateUpdate (name, expr)

  return [name, assign, expr]

printst :: ParsecT [Token] State IO [Token]
printst = do
  id <- printFun
  beginpToken
  expr <- expression

  liftIO $ putStr . show $ expr

  endpToken
  return []

println :: ParsecT [Token] State IO [Token]
println = do
  id <- printlnFun
  beginpToken
  expr <- expression

  liftIO $ print expr

  endpToken
  return []

types :: ParsecT [Token] State IO Token
types = intToken <|> floatToken <|> boolToken <|> charToken <|> stringToken

decls :: ParsecT [Token] State IO [Token]
decls = do
  decl <- varDecl
  rdecls <- remainingDecls
  return $ decl ++ rdecls

remainingDecls :: ParsecT [Token] State IO [Token]
remainingDecls =
  ( do
      a <- varDecl
      b <- remainingDecls
      return $ a ++ b
  )
    <|> return []

statements :: ParsecT [Token] State IO [Token]
statements = do
  st <- try println <|> printst <|> assign
  sts <- remainingStatements
  return $ st ++ sts

remainingStatements :: ParsecT [Token] State IO [Token]
remainingStatements =
  ( do
      st <- try println <|> printst <|> assign
      sts <- remainingStatements
      return $ st ++ sts
  )
    <|> return []

program :: ParsecT [Token] State IO [Token]
program = do
  p <- moduleToken
  pn <- idToken
  d <- many decls
  st <- many statements
  eof
  return $ [p, pn] ++ concat d ++ concat st

parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program [] "Error"
