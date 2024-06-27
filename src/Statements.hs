module Statements where

-- Interpreter Modules

import Builtin
---------------------------------

-- Haskell Imports
import Control.Monad (when, unless)
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

recordValue :: String -> ParsecT [Token] State IO Type
recordValue recordName = do
    begin <- beginBToken
    fields <- fieldValues recordName
    end <- endBToken
    state <- getState
    let typeTable = getTypes state
    let definedFields = getRecordFields recordName typeTable
    validateFields definedFields fields
    return $ R recordName fields

validateFields :: [(Token, Type)] -> [(Token, Token)] -> ParsecT [Token] State IO ()
validateFields definedFields providedFields = do
    let definedNames = map (name . fst) definedFields
    let providedNames = map (name . fst) providedFields
    let missingFields = filter (`notElem` providedNames) definedNames
    unless (null missingFields) $
        parserFail $ "Missing fields: " ++ show missingFields

fieldValues :: String -> ParsecT [Token] State IO [(Token, Token)]
fieldValues recordName = do
    field <- fieldValue recordName
    fields <- remainingFieldValues recordName
    return (field : fields)

remainingFieldValues :: String -> ParsecT [Token] State IO [(Token, Token)]
remainingFieldValues recordName = 
  ( do
      comma <- commaToken
      field <- fieldValue recordName
      fields <- remainingFieldValues recordName
      return (field : fields)
  ) 
  <|> return []

fieldValue :: String -> ParsecT [Token] State IO (Token, Token)
fieldValue recordName = do
    id <- idToken
    arrow <- arrowToken
    state <- getState
    (v, expr) <- getSt <|> expression
    let typeTable = getTypes state
    let expected_type = typeof' $ findField (name id) (getRecordFields recordName typeTable)
    let actual_type = typeof v 
    if (expected_type /= actual_type) then error $ "Type mismatch for field: " 
                                                    ++ (name id) ++ " expected type: " 
                                                    ++ show expected_type 
                                                    ++ " actual type: " ++ show actual_type
    else return (id, v)

findField :: String -> [(Token, Type)] -> Type
findField field [] = error $ "Field not found: " ++ field
findField field ((token, typ):xs) = if field == name token then typ else findField field xs

expressionValue :: ParsecT [Token] State IO Type
expressionValue = do
    expr <- expression
    case expr of
      (LiteralValue _ val, _) -> return val
      _ -> parserFail "Invalid value for record field"

fields :: Type -> [(Token, Token)]
fields (R _ fs) = fs
fields _ = error "Not a record type"

recordAssignment :: String -> ParsecT [Token] State IO (Token, [Token])
recordAssignment recordName = do
  record <- recordValue recordName
  return (LiteralValue (pos (fst (head (fields record)))) record, [])

getRecordFields :: String -> TypeTable -> [(Token, Type)]
getRecordFields recordName types =
  case lookup recordName types of
    Just fields -> fields
    Nothing -> error $ "Record type " ++ recordName ++ " not found"