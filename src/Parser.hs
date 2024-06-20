{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import Builtin
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IntMap (update)
import Expressions
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
  colonToken
  decltype <- types
  assignToken
  expr <- getst <|> expression

  (flag, symt, stack, types, subp) <- getState
  if not flag
    then
      return
        [ modifier,
          id,
          colon,
          decltype,
          assign,
          expr
        ]
    else do
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
          "Type mismatch at "
            ++ show (pos id)
            ++ ".\n"
            ++ "Expected "
            ++ expected_type
            ++ ", got "
            ++ actual_type
            ++ ".\n"

      setState
        ( flag,
          symTableInsert (name id) (modifier, decltype, expr) symt,
          stack,
          scope,
          types,
          subp
        )

      return
        [ modifier,
          id,
          colon,
          decltype,
          assign,
          expr
        ]

assign :: ParsecT [Token] State IO [Token]
assign = do
  id <- idToken
  assign <- assignToken
  expr <- getst <|> expression

  (flag, symt, stack, scope, types, subp) <- getState

  when flag $
    setState
      ( flag,
        symTableUpdate (name id) expr symt,
        stack,
        scope,
        types,
        subp
      )

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
      (GetInt p) -> IntL p $ parseInput p input
      (GetFloat p) -> FloatL p $ parseInput p input
      (GetChar p) -> CharL p $ parseInput p input
      (GetString p) -> StringL p input

parseInput :: (Read a) => Pos -> String -> a
parseInput p = check p . readMaybe
  where
    check p (Just x) = x
    check p Nothing = error $ "Couldn't parse input at " ++ show p ++ ". Maybe the type doesn't match?\n"

types :: ParsecT [Token] State IO Token
types = intToken <|> floatToken <|> boolToken <|> charToken <|> stringToken

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

blockParser :: String -> String -> ParsecT [Token] State IO [Token]
blockParser static_parent name = do
  beginBToken

  (flag, symt, stack, scope, types, subp) <- getState
  setState
    ( flag,
      symt,
      if flag
        then pushIntoStack name stack
        else stack,
      if flag && name /= "_main_"
        then scope
        else (name, static_parent) : scope,
      types,
      subp
    )

  lines <- many (many decls <|> many statements)

  return $ (concat . concat) lines

mainProgram :: ParsecT [Token] State IO [Token]
mainProgram = do
  fun <- funToken
  main <- mainFun
  beginpToken >> endpToken

  updateState $ setFlag True

  block <- blockParser "_global_" "_main_"

  return $ main : block

program :: ParsecT [Token] State IO [Token]
program = do
  p <- moduleToken
  pn <- idToken
  d <- many decls
  main <- mainProgram
  eof
  return $ [p, pn] ++ concat d

parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program defaultState "Error"
