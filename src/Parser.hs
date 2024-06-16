{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import Control.Monad.IO.Class (liftIO)
import Data.IntMap (update)
import Expressions
import Lexer
import State
import Text.Parsec hiding (State)
import Tokens

varDecl :: ParsecT [Token] State IO [Token]
varDecl = do
  modifier <- letToken <|> mutToken
  name <- idToken
  colon <- colonToken
  decltype <- types
  assign <- assignToken
  expr <- expression

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
  st <- assign
  sts <- remainingStatements
  return $ st ++ sts

remainingStatements :: ParsecT [Token] State IO [Token]
remainingStatements =
  ( do
      st <- assign
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
