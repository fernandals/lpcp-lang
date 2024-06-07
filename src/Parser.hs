{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import Control.Monad.IO.Class (liftIO)
import Data.IntMap (update)
import Lexer
import State
import Text.Parsec hiding (State)
import Tokens

varDecl :: ParsecT [Token] State IO [Token]
varDecl = do
  modifier <- letToken <|> mutToken
  name <- idToken
  colon <- colonToken
  decltype <- intToken <|> floatToken
  assign <- assignToken
  expr <- try binArithExpr

  updateState $ stateInsert (modifier, decltype, name, expr)
  σ <- getState
  liftIO $ print σ

  return
    [ modifier,
      name,
      colon,
      decltype,
      assign,
      expr
    ]

-- Expressoes aritmeticas Testes

atomExpr :: ParsecT [Token] State IO(Token)
atomExpr = do 
   n <- intLToken <|> floatLToken <|> intToken <|> floatToken 
   return (n)

binArithExpr :: ParsecT [Token] State IO(Token)
binArithExpr = do
   n1 <- termArithExpr
   result <- evalBinRemaining n1
   return (result)

evalBinRemaining :: Token -> ParsecT [Token] State IO(Token)
evalBinRemaining n1 = do
  op <- plusToken <|> minusToken
  n2 <- termArithExpr
  result <- evalBinRemaining (evalArith n1 op n2)
  return (result) 
  <|> return (n1) 

termArithExpr :: ParsecT [Token] State IO(Token)
termArithExpr = do
   n1 <- powArithExpr
   result <- evalTermRemaining n1
   return (result)

evalTermRemaining :: Token -> ParsecT [Token] State IO(Token)
evalTermRemaining n1 = do
  op <- timesToken <|> dividesToken
  n2 <- powArithExpr
  result <- evalTermRemaining (evalArith n1 op n2)
  return (result) 
  <|> return (n1) 

powArithExpr :: ParsecT [Token] State IO(Token)
powArithExpr = do
   n1 <- try baseArithExpr <|> atomExpr
   result <- evalPowRemaining n1
   return (result)

evalPowRemaining :: Token -> ParsecT [Token] State IO(Token)
evalPowRemaining n1 = do
  op <- powToken
  n2 <- try baseArithExpr <|>  atomExpr
  result <- evalPowRemaining (evalArith n1 op n2)
  return (result) 
  <|> return (n1) 

baseArithExpr :: ParsecT [Token] State IO(Token)
baseArithExpr = do
  l <- beginpToken
  expr <- binArithExpr
  r <- endpToken
  return (expr)

evalBaseRemaining :: Token -> ParsecT [Token] State IO(Token)
evalBaseRemaining n1 = do
  op <- powToken
  n2 <- atomExpr
  result <- evalBaseRemaining (evalArith n1 op n2)
  return (result) 
  <|> return (n1) 

evalArith :: Token -> Token -> Token -> Token
evalArith (IntL p x) (Plus _) (IntL r y) = IntL p (x + y)
evalArith (IntL p x) (Minus _) (IntL r y) = IntL p (x - y)
evalArith (IntL p x) (Times _) (IntL r y) = IntL p (x * y)
evalArith (IntL p x) (Divides _) (IntL r y) = IntL p (x `div` y)
evalArith (IntL p x) (Pow _) (IntL r y) = IntL p (x ^ y)


evalVar :: ParsecT [Token] State IO Token
evalVar = do
  id <- intToken
  σ <- getState
  return (getValue id σ)
-- ---------------------------------------------------

assign :: ParsecT [Token] State IO [Token]
assign = do
  name <- idToken
  assign <- assignToken
  expr <- try binArithExpr

  updateState $ stateUpdate (name, expr)
  σ <- getState
  liftIO $ print σ

  return [name, assign, expr]

-- TODO
-- funDecl :: ParsecT [Token] State IO [Token]
-- funDecl = _

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
  st <- assign
  sts <- remainingStatements
  return $ st ++ sts

remainingStatements :: ParsecT [Token] State IO [Token]
remainingStatements =
  ( do
      st <- assign
      sts <- remainingStatements
      return $ st ++ sts
  )
    <|> return []

program :: ParsecT [Token] State IO [Token]
program = do
  p <- moduleToken
  pn <- idToken
  d <- decls
  st <- statements
  eof
  return $ [p, pn] ++ d ++ st

parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program [] "Error"
