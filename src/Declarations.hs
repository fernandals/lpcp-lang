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
types = intToken <|> floatToken <|> boolToken <|> charToken <|> stringToken <|> listType

listType :: ParsecT [Token] State IO Token
listType = do
  l <- beginSBToken
  t <- types
  r <- endSBToken
  return (List (pos l) t)

changeTypeOfList :: Token -> Token -> Token
changeTypeOfList (LiteralValue p (L t i l)) (List p' t') = (LiteralValue p (L t' i l))
changeTypeOfList _ _ = error $ "aquilo nao eh uma lista"

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
      (v', expr) <- getSt <|> expression
      let expected_type = typeof decltype
      let v = v'
      let emp = (isEmpty v') && (isList decltype) 
      let actual_type = if emp then typeof decltype else typeof v'
      let v = if emp then changeTypeOfList v' decltype else v'

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
