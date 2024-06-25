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

recordDecls :: ParsecT [Token] State IO [Token]
recordDecls = do
    record <- recordToken
    id <- idToken
    begin <- beginBToken
    fields <- fieldDecls
    end <- endBToken

    state <- getState

    updateState $ typeInsert (name id)
    return $ [record, id, begin] ++ fields ++ [end]

fieldDecls :: ParsecT [Token] State IO [Token]
fieldDecls = do
    field <- fieldDecl
    fields <- remainingFieldDecls
    return $ field ++ fields

remainingFieldDecls :: ParsecT [Token] State IO [Token]
remainingFieldDecls = 
  ( do
      comma <- commaToken
      field <- fieldDecl
      fields <- remainingFieldDecls
      return $ [comma] ++ field ++ fields
  ) 
  <|> return []

fieldDecl :: ParsecT [Token] State IO [Token]
fieldDecl = do
    name <- idToken
    colon <- colonToken
    typ <- types
    return [name, colon, typ]