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
import Text.Read
import Tokens
import Utils

varDecl :: ParsecT [Token] State IO [Token]
varDecl = do
  modifier <- letToken <|> mutToken
  name <- idToken
  colon <- colonToken
  decltype <- types
  assign <- assignToken
  expr <- getst <|> expression

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
  expr <- getst <|> expression

  updateState $ stateUpdate (name, expr)

  return [name, assign, expr]

printst :: ParsecT [Token] State IO [Token]
printst =
  do
    comm <- printFun <|> printLnFun
    beginpToken

    expr <- expression
    liftIO
      $ case comm of
        (Print _) -> putStr . show
        (PrintLn _) -> print
      $ expr

    endpToken
    return [comm, expr]

getst :: ParsecT [Token] State IO Token
getst =
  do
    comm <- getIntFun <|> getFloatFun <|> getCharFun <|> getStringFun
    beginpToken >> endpToken

    input <- liftIO getLine
    pure $ case comm of
      (GetInt p) -> IntL p $ parseInput input
      (GetFloat p) -> FloatL p $ parseInput input
      (GetChar p) -> CharL p $ parseInput input
      (GetString p) -> StringL p input

parseInput :: (Read a) => String -> a
parseInput = check . readMaybe
  where
    check (Just x) = x
    check Nothing = error "Couldn't parse input. Maybe the type doesn't match?\n"

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
  st <- printst <|> assign
  sts <- remainingStatements
  return $ st ++ sts

remainingStatements :: ParsecT [Token] State IO [Token]
remainingStatements =
  ( do
      st <- printst <|> assign
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
