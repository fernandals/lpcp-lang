module FluxControl where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Binary.Get (remaining)
import Declarations
import Expressions
import ExpressionsParser
import Lexer
import State
import Statements
import Text.Parsec hiding (State)
import Tokens
import Utils

blockParser :: ParsecT [Token] State IO [Token]
blockParser = do
  begin <- beginBToken

  lines <- many (decls <|> statements <|> ifParser <|> whileParser)

  end <- endBToken

  return $ begin : concat lines ++ [end]

ifParser :: ParsecT [Token] State IO [Token]
ifParser = do
  (flag, _, _, _, _) <- getState

  if flag
    then do ifSt
    else do
      if_tk <- ifToken
      expr <- binExpr
      block <- blockParser

      elif_st <- many elifParser
      else_st <- option [] elseParser

      return $ if_tk : expr ++ block ++ concat elif_st ++ else_st

elifParser :: ParsecT [Token] State IO [Token]
elifParser = do
  elif_tk <- elifToken
  expr <- binExpr
  block <- blockParser

  return $ elif_tk : expr ++ block

elseParser :: ParsecT [Token] State IO [Token]
elseParser = do
  else_tk <- elseToken
  block <- blockParser

  return $ else_tk : block

whileParser :: ParsecT [Token] State IO [Token]
whileParser = do
  (flag, _, _, _, _) <- getState
  if flag
    then do whileSt
    else do
      while_tk <- whileToken
      expr <- binExpr
      do_tk <- doToken
      block <- blockParser

      return $ while_tk : expr ++ do_tk : block

-- Semantics
ifSt :: ParsecT [Token] State IO [Token]
ifSt = do
  if_tk <- ifToken
  (v, expr) <- expression

  if isTrue v
    then do
      (_, _, (act_name, _) : _, _, _) <- getState
      let scope_name = scopeNameBlock act_name "if"
      updateState $ pushStack scope_name
      block <- blockParser
      updateState $ symTableCleanScope scope_name
      updateState popStack

      updateState $ setFlag False
      elif_st <- many elifParser
      else_st <- option [] elseParser

      updateState $ setFlag True

      return $ if_tk : expr ++ block ++ concat elif_st ++ else_st
    else do
      updateState $ setFlag False
      block <- blockParser
      updateState $ setFlag True
      next_st <- try elifSt <|> option [] elseSt

      return $ if_tk : expr ++ block ++ next_st

elifSt :: ParsecT [Token] State IO [Token]
elifSt = do
  elif_tk <- elifToken
  (v, expr) <- expression

  if isTrue v
    then do
      (_, _, (act_name, _) : _, _, _) <- getState
      let scope_name = scopeNameBlock act_name "elif"
      updateState $ pushStack scope_name
      block <- blockParser
      updateState $ symTableCleanScope scope_name
      updateState popStack

      updateState $ setFlag False
      elif_st <- many elifParser
      else_st <- option [] elseParser
      updateState $ setFlag True

      return $ elif_tk : expr ++ block ++ concat elif_st ++ else_st
    else do
      updateState $ setFlag False
      block <- blockParser
      updateState $ setFlag True
      next_st <- try elifSt <|> option [] elseSt

      return $ elif_tk : expr ++ block ++ next_st

elseSt :: ParsecT [Token] State IO [Token]
elseSt = do
  else_tk <- elseToken
  (_, _, (act_name, _) : _, _, _) <- getState
  let scope_name = scopeNameBlock act_name "else"
  updateState $ pushStack scope_name
  block <- blockParser
  updateState $ symTableCleanScope scope_name
  updateState popStack
  return $ else_tk : block

-- caso v é true, parsear o bloco (como no if)
whileSt :: ParsecT [Token] State IO [Token]
whileSt = do
  fixp <- getInput
  while_tk <- whileToken
  (v, expr) <- expression
  do_tk <- doToken

  (_, _, (act_name, _) : _, _, _) <- getState
  if isTrue v
    then do
      let scope_name = scopeNameBlock act_name "while"
      updateState $ pushStack scope_name
      block <- blockParser
      updateState $ symTableCleanScope scope_name
      updateState popStack
      setInput fixp
      return $ while_tk : expr ++ do_tk : block
    else do
      updateState $ setFlag False
      block <- blockParser
      updateState $ setFlag True
      return $ while_tk : expr ++ do_tk : block
