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
types = try intToken <|> try floatToken <|> try boolToken <|> try charToken <|> try stringToken <|> recordType

recordType :: ParsecT [Token] State IO Token 
recordType = do
  id <- idToken
  state <- getState
  let types = getTypes state
  case types of 
      [] -> parserFail $ "Unknown record type: " ++ name id
      _ -> case lookup (name id) types of
              Just fields -> return (RecordType (pos id) id)
              Nothing -> parserFail $ "Unknown record type: " ++ name id

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
      (v, expr) <- getSt <|> expression <|> recordAssignment (getRecordName decltype)
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

    updateState (typeInsert ((name id), fields))
    return $ [record, id, begin, end]

fieldDecls :: ParsecT [Token] State IO [(Token, Type)]
fieldDecls = do
    field <- fieldDecl
    fields <- remainingFieldDecls
    return $ field ++ fields

remainingFieldDecls :: ParsecT [Token] State IO [(Token, Type)]
remainingFieldDecls = 
  ( do
      comma <- commaToken
      field <- fieldDecl
      fields <- remainingFieldDecls
      return $ field ++ fields
  ) 
  <|> return []

fieldDecl :: ParsecT [Token] State IO [(Token, Type)]
fieldDecl = do
    name <- idToken
    colon <- colonToken
    typ <- types
    return [(name, tokenToType typ)]