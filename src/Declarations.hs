module Declarations where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Errors
import Expressions
import ExpressionsParser
import Lexer
import State
import Statements
import Text.Parsec hiding (State)
import Tokens
import Utils

-- Top Level
decls :: ParsecT [Token] State IO [Token]
decls = do
  varDecl

-- Types involved in declarations
types :: ParsecT [Token] State IO Token
types = intToken <|> floatToken <|> boolToken <|> charToken <|> stringToken

paramDecl :: ParsecT [Token] State IO (Token, Token)
paramDecl = do
  param_name <- idToken
  colonToken
  param_type <- types
  return (param_name, param_type)

returnDecl :: ParsecT [Token] State IO [Token]
returnDecl = do
  arrow <- arrowToken
  return_type <- types
  return [arrow, return_type]

funDecl :: ParsecT [Token] State IO [Token]
funDecl = do
  fun_tk <- funToken
  fun_name <- idToken
  lp <- beginpToken
  params <- many paramDecl
  arr <- option [] returnDecl

varDecl :: ParsecT [Token] State IO [Token]
varDecl = do
  modifier <- letToken <|> mutToken
  id <- idToken
  colon <- colonToken
  decltype <- types
  assign <- assignToken

  (flag, _, (act_name, _) : _, _, _) <- getState

  if canExecute flag act_name
    then do
      (v, expr) <- getSt <|> expression
      let expected_type = typeof decltype
      let actual_type = typeof v

      when (expected_type /= actual_type) $
        error $
          typeErrorMsg (pos id) decltype v

      updateState $ symTableInsert (scopeNameVar act_name (name id)) (modifier, decltype, v)

      return $ modifier : id : colon : decltype : assign : expr
    else do
      expr <- getStSyntactic <|> binExpr
      return $
        [ modifier,
          id,
          colon,
          decltype,
          assign
        ]
          ++ expr
