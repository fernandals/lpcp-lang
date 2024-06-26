module Statements where

-- Interpreter Modules

import Builtin
---------------------------------

-- Haskell Imports
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import Expressions
import ExpressionsParser
import Lexer
import State
import System.IO (hFlush, stdout)
import Text.Parsec hiding (State)
import Text.Read (get, readMaybe)
import Tokens
import Utils
import Errors

-- Top Level
statements :: ParsecT [Token] State IO [Token]
statements = try assignIndex <|> assignSt <|> printSt <|> printfSt

assignIndex :: ParsecT [Token] State IO [Token]
assignIndex = do
  list_name <- idToken
  sbl <- beginSBToken
  (index, exprIndex) <- expression
  sbr <- endSBToken
  ids <- many many_indexs
  assign <- assignToken

  let l_name = name list_name
  let l_p = pos list_name
  
  let allIdExprs = exprIndex ++ (concatMap snd ids)

  let listIds = index : map fst ids

  state@(flag, symt, (act_name, _) : stack, types, subp) <- getState

  if canExecute flag act_name
    then do
      let val = symTableGetVal (scopeNameVar act_name l_name) l_p state

      (v, expr) <- getSt <|> expression
      v' <- tokenToType (v, expr)

      let newVal =  setMat listIds val v -- canInsert val index v'
      updateState $ symTableUpdate (scopeNameVar act_name (name list_name)) newVal
      return $ list_name : sbl : allIdExprs ++ [sbr] ++ [assign] ++ expr
    else do
      expr <- getStSyntactic <|> binExpr
      return $ list_name : sbl : allIdExprs ++ [sbr] ++ [assign] ++ expr

-- O caos abaixo eh a insercao de um valor numa matriz

setMat :: [Token] -> Token -> Token -> Token
setMat (i:is) (LiteralValue p (L t len [])) v = error $ "Error: Attempt to access an index in an empty matrix or list at position " ++ show (pos i)
setMat (i:is) (LiteralValue p (L t len (x:xs))) v 
  | null is = setList' i (LiteralValue p (L t len (x:xs))) v
  | otherwise = 
    case x of 
      L t' len ls -> setList' i (LiteralValue p (L t len (x:xs))) (setMat is (getList i (LiteralValue p (L t len (x:xs)))) v)
      _ -> error $ "Error: Index out of bounds at position " ++ show (pos i)

setList' :: Token -> Token -> Token -> Token
setList' (LiteralValue p' (I i)) (LiteralValue p (L t len l)) value 
  | i < 0          = error $ "Error: Negative index at position " ++ show p' 
  | i > len        = error $ outOfBounds p len
  | otherwise      = LiteralValue p (L t len (go i l))
  where
    go 0 (x:xs) = if typeof' x == typeof' (tokenToType' value)
      then (tokenToType' value) : xs
      else error $ "Error: Type mismatch at " ++ show (pos value)
    go n (x: xs) = x : go (n-1) xs
    go _ [] = []
setList' y  (LiteralValue p (L t len l)) value = error $ nonIntegerIndex (pos y)
setList' _ y value  = error $ "Error: Attempt to insert an element non-list object"

getList :: Token -> Token -> Token
getList (LiteralValue p' (I i)) (LiteralValue p (L t len l)) =
      if i < length l
        then LiteralValue p $ l !! i
        else error $ "lista grande"
getList _ _ = error $ "nao eh um indice inteiro"

tokenToType' :: Token -> Type
tokenToType' (LiteralValue p (I i)) =  I i
tokenToType' (LiteralValue p (F f)) = F f
tokenToType' (LiteralValue p (C c)) =  C c
tokenToType' (LiteralValue p (S s)) = S s
tokenToType' (LiteralValue p (L t i l)) = L t i l

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

        return $ comm : lp : intersperse (Comma (0, 0)) (concatMap snd args ++ [rp])
      else do
        lp <- beginpToken
        args <- binExpr `sepBy` commaToken
        rp <- endpToken
        return $ comm : lp : intersperse (Comma (0, 0)) (concat args ++ [rp])

-- Input Statements
-- getInt() | getFloat | getChar | getString
getSt :: ParsecT [Token] State IO (Token, [Token])
getSt = do
  comm <- getIntFun <|> getFloatFun <|> getCharFun <|> getStringFun
  lp <- beginpToken
  rp <- endpToken

  input <- liftIO getLine

  return $ (,[comm, lp, rp]) $ case comm of
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
