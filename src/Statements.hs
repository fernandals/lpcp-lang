module Statements where

-- Interpreter Modules

import Builtin
---------------------------------

-- Haskell Imports
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Expressions
import ExpressionsParser
import Lexer
import State
import System.IO (hFlush, stdout)
import Text.Parsec hiding (State)
import Text.Read (get, readMaybe)
import Tokens
import Utils

-- Top Level
statements :: ParsecT [Token] State IO [Token]
statements = assignSt <|> printSt <|> printfSt

assignSt :: ParsecT [Token] State IO [Token]
assignSt = do
  id <- idToken
  assign <- assignToken
  expr <- getSt <|> expression

  (flag, symt, (act_name, _) : stack, types, subp) <- getState
  if canExecute flag act_name
    then do
      expr <- getSt <|> expression
      updateState $ symTableUpdate (scopeNameVar act_name (name id)) expr
      return []
    else do
      expr <- getStSyntactic <|> binExpr
      return $ id : assign : expr

printSt :: ParsecT [Token] State IO [Token]
printSt =
  do
    comm <- printFun <|> printLnFun

    (flag, _, (act_name, _) : _, _, _) <- getState
    if canExecute flag act_name
      then do
        beginpToken
        expr <- expression
        liftIO
          $ case comm of
            (Print _) -> putStr . show
            (PrintLn _) -> print
          $ expr
        liftIO $ hFlush stdout
        endpToken
        return []
      else do
        lp <- beginpToken
        expr <- binExpr
        rp <- endpToken
        return $ lp : expr ++ [rp]

printfSt :: ParsecT [Token] State IO [Token]
printfSt =
  do
    comm <- printFFun

    (flag, _, (act_name, _) : _, _, _) <- getState
    if canExecute flag act_name
      then do
        lp <- beginpToken
        args <- binExpr `sepBy` commaToken
        rp <- endpToken
        return $ lp : concat args ++ [rp]
      else do
        beginpToken
        args <- binExpr `sepBy` commaToken
        endpToken

        liftIO $
          putStrLn $
            foldr1 (++) (show <$> args)

        return []

-- Input Statements
-- getInt() | getFloat | getChar | getString
getSt :: ParsecT [Token] State IO Token
getSt =
  do
    comm <- getIntFun <|> getFloatFun <|> getCharFun <|> getStringFun
    beginpToken >> endpToken

    input <- liftIO getLine
    pure $ case comm of
      (GetInt p) -> LiteralValue p (I $ parseInput p input)
      (GetFloat p) -> LiteralValue p (F $ parseInput p input)
      (GetChar p) -> LiteralValue p (C $ parseInput p input)
      (GetString p) -> LiteralValue p (S input)

getStSyntactic :: ParsecT [Token] State IO [Token]
getStSyntactic = do
  comm <- getIntFun <|> getFloatFun <|> getCharFun <|> getStringFun
  lp <- beginpToken
  rp <- endpToken

  return [comm, lp, rp]

parseInput :: (Read a) => Pos -> String -> a
parseInput p = check p . readMaybe
  where
    check p (Just x) = x
    check p Nothing = error $ "Couldn't parse input at " ++ show p ++ ". Maybe the type doesn't match?\n"
