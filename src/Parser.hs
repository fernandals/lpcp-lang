{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import Builtin
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IntMap (update)
import Expressions
import ParserExpressions
import GHC.IO.FD (stdout)
import Lexer
import State
import System.IO
import Text.Parsec hiding (State)
import Text.Printf (printf)
import Text.Read
import Tokens
import Utils

varDecl :: ParsecT [Token] State IO [Token]
varDecl = do
  modifier <- letToken <|> mutToken
  id <- idToken
  colon <- colonToken
  decltype <- types
  assign <- assignToken

  (flag, symt, (act_name, _) : stack, types, subp) <- getState

  if not flag
    then do
      expr <- binExpr
      liftIO $ print expr
      return $
        [ modifier,
          id,
          colon,
          decltype,
          assign ] 
          ++ expr

    else do
      expr <- getst <|> expression
      let expected_type = typeof decltype
      let actual_type = typeof expr

      when (actual_type == "error") $
        error $
          "Type mismatch in expression evaluation at "
            ++ show (pos id)
            ++ ".\n"
            ++ "Check the types of your operands.\n"
      when (expected_type /= actual_type) $
        error $
          typeErrorMsg (pos id) decltype expr

      updateState $ symTableInsert (scopeNameVar act_name (name id)) (modifier, decltype, expr)

      return
        [ modifier,
          id,
          colon,
          decltype,
          assign,
          expr ]
          
        

assign :: ParsecT [Token] State IO [Token]
assign = do
  id <- idToken
  assign <- assignToken
  expr <- getst <|> expression

  (flag, symt, (act_name, _) : stack, types, subp) <- getState

  when flag $
    updateState $
      symTableUpdate (scopeNameVar act_name (name id)) expr

  return [id, assign, expr]

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
    liftIO $ hFlush System.IO.stdout

    endpToken
    return [comm, expr]

printfst :: ParsecT [Token] State IO [Token]
printfst =
  do
    comm <- printFFun
    beginpToken
    args <- expression `sepBy` commaToken
    -- liftIO $ print args
    liftIO $
      putStrLn $
        foldr1 (++) (show <$> args)

    endpToken
    return [comm]

getst :: ParsecT [Token] State IO Token
getst =
  do
    comm <- getIntFun <|> getFloatFun <|> getCharFun <|> getStringFun
    beginpToken >> endpToken

    input <- liftIO getLine
    pure $ case comm of
      (GetInt p) -> LiteralValue p (I $ parseInput p input)
      (GetFloat p) -> LiteralValue p (F $ parseInput p input)
      (GetChar p) -> LiteralValue p (C $ parseInput p input)
      (GetString p) -> LiteralValue p (S input)

parseInput :: (Read a) => Pos -> String -> a
parseInput p = check p . readMaybe
  where
    check p (Just x) = x
    check p Nothing = error $ "Couldn't parse input at " ++ show p ++ ". Maybe the type doesn't match?\n"

types :: ParsecT [Token] State IO Token
types = intToken <|> floatToken <|> boolToken <|> charToken <|> stringToken

globalVarDecl :: ParsecT [Token] State IO [Token]
globalVarDecl = do
  modifier <- letToken <|> mutToken
  id <- idToken
  colonToken
  decltype <- types
  assignToken
  expr <- getst <|> expression

  let expected_type = typeof decltype
  let actual_type = typeof expr

  when (actual_type == "error") $
    error $
      "Type mismatch in expression evaluation at "
        ++ show (pos id)
        ++ ".\n"
        ++ "Check the types of your operands.\n"
  when (expected_type /= actual_type) $
    error $
      typeErrorMsg (pos id) decltype expr

  updateState $ symTableInsert (scopeNameVar "_global_" (name id)) (modifier, decltype, expr)

  return [modifier, id, decltype, expr]

globalDecls :: ParsecT [Token] State IO [Token]
globalDecls = do
  globalVarDecl

decls :: ParsecT [Token] State IO [Token]
decls = do
  varDecl

-- rdecls <- remainingDecls
-- return $ decl ++ rdecls

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
  st <- printfst <|> printst <|> assign
  sts <- remainingStatements
  return $ st ++ sts

remainingStatements :: ParsecT [Token] State IO [Token]
remainingStatements =
  ( do
      st <- printfst <|> printst <|> assign
      sts <- remainingStatements
      return $ st ++ sts
  )
    <|> return []

blockParser :: String -> ParsecT [Token] State IO [Token]
blockParser name = do
  beginBToken

  lines <- many (many1 decls <|> many1 statements)

  endBToken
  return $ (concat . concat) lines

mainProgram :: ParsecT [Token] State IO [Token]
mainProgram = do
  fun <- funToken
  main <- mainFun
  beginpToken >> endpToken

  let scope_name = scopeNameBlock "_global_" "main"

  updateState $ setFlag True
  updateState $ pushStack scope_name

  block <- blockParser scope_name

  return $ main : block

program :: ParsecT [Token] State IO [Token]
program = do
  p <- moduleToken
  pn <- idToken
  d <- many globalDecls
  main <- mainProgram
  eof
  return $ [p, pn] ++ concat d

parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program defaultState "Error"
