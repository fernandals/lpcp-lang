{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import Control.Monad.IO.Class (liftIO)
import Data.IntMap (update)
import Lexer
import State
import Text.Parsec hiding (State)
import Tokens
import Expressions

varDecl :: ParsecT [Token] State IO [Token]
varDecl = do
  modifier <- letToken <|> mutToken
  name <- idToken
  colon <- colonToken
  decltype <- intToken <|> floatToken <|> boolToken
  assign <- assignToken
  expr <- try binBoolExpr <|> unaBoolExpr <|> binArithExpr <|> unaArithExpr

  updateState $ stateInsert (modifier, decltype, name, expr)
  σ <- getState
  liftIO $ print σ

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
  expr <- try binArithExpr

  updateState $ stateUpdate (name, expr)
  σ <- getState
  liftIO $ print σ

  return [name, assign, expr]

-- TODO
-- funDecl :: ParsecT [Token] State IO [Token]
-- funDecl = _

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
  d <- decls
  st <- statements
  eof
  return $ [p, pn] ++ d ++ st

parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program [] "Error"
