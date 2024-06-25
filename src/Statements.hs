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

  (flag, symt, (act_name, _) : stack, types, subp) <- getState
  if canExecute flag act_name
    then do
      (v, expr) <- getSt <|> expression
      updateState $ symTableUpdate (scopeNameVar act_name (name id)) v
      return $ id : assign : expr
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
        lp <- beginpToken
        (v, expr) <- expression
        liftIO
          $ case comm of
            (Print _) -> putStr . show
            (PrintLn _) -> print
          $ v
        liftIO $ hFlush stdout
        rp <- endpToken
        return $ comm : lp : expr ++ [rp]
      else do
        lp <- beginpToken
        expr <- binExpr
        rp <- endpToken
        return $ comm : lp : expr ++ [rp]

printfSt :: ParsecT [Token] State IO [Token]
printfSt =
  do
    comm <- printFFun

    (flag, _, (act_name, _) : _, _, _) <- getState
    if canExecute flag act_name
      then do
        lp <- beginpToken
        args <- expression `sepBy` commaToken
        rp <- endpToken

        liftIO $
          putStrLn $
            foldr1 (++) (show . fst <$> args)

        return $ comm : lp : concatMap snd args ++ [rp]
      else do
        lp <- beginpToken
        args <- binExpr `sepBy` commaToken
        rp <- endpToken
        return $ comm : lp : concat args ++ [rp]

-- Input Statements
-- getInt() | getFloat | getChar | getString
getSt :: ParsecT [Token] State IO (Token, [Token])
getSt = do
  comm <- getIntFun <|> getFloatFun <|> getCharFun <|> getStringFun
  lp <- beginpToken
  rp <- endpToken

  input <- liftIO getLine
  
  let value = case comm of
        (GetInt p) -> LiteralValue p (I $ parseInput p input)
        (GetFloat p) -> LiteralValue p (F $ parseInput p input)
        (GetChar p) -> LiteralValue p (C $ parseInput p input)
        (GetString p) -> LiteralValue p (S input)

  return (value, [comm, lp, rp])


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
