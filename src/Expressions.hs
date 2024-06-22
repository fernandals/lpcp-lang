{-# LANGUAGE UnicodeSyntax #-}

module Expressions where

import Builtin
import Errors
import ExpressionsEvaluation
import Lexer
import State
import Text.Parsec hiding (State)
import Tokens
import Utils

expression :: ParsecT [Token] State IO Token
expression = term >>= expressionRemaining

expressionRemaining :: Token -> ParsecT [Token] State IO Token
expressionRemaining n1 =
  ( do
      op <- orToken <|> xorToken
      n2 <- term
      expressionRemaining $ eval n1 op n2
  )
    <|> return n1

term :: ParsecT [Token] State IO Token
term = notFactor >>= termRemaining

termRemaining :: Token -> ParsecT [Token] State IO Token
termRemaining n1 =
  ( do
      op <- andToken
      n2 <- notFactor
      termRemaining $ eval n1 op n2
  )
    <|> return n1

notFactor :: ParsecT [Token] State IO Token
notFactor = try unaBoolExpr <|> factor

factor :: ParsecT [Token] State IO Token
factor = subExpression >>= factorRemaining

factorRemaining :: Token -> ParsecT [Token] State IO Token
factorRemaining n1 =
  ( do
      rel <- leqToken <|> geqToken <|> lessToken <|> greaterToken <|> eqToken <|> neqToken
      n2 <- subExpression
      factorRemaining $ eval n1 rel n2
  )
    <|> return n1

subExpression :: ParsecT [Token] State IO Token
subExpression = subTerm >>= evalSubExpressionRemaining

evalSubExpressionRemaining :: Token -> ParsecT [Token] State IO Token
evalSubExpressionRemaining n1 =
  ( do
      op <- plusToken <|> minusToken
      n2 <- subTerm
      evalSubExpressionRemaining (eval n1 op n2)
  )
    <|> return n1

subTerm :: ParsecT [Token] State IO Token
subTerm = negSubFactor >>= evalSubTermRemaining

evalSubTermRemaining :: Token -> ParsecT [Token] State IO Token
evalSubTermRemaining n1 =
  ( do
      op <- timesToken <|> dividesToken <|> modulosToken
      n2 <- negSubFactor
      evalSubTermRemaining (eval n1 op n2)
  )
    <|> return n1

negSubFactor :: ParsecT [Token] State IO Token -- pode dar ruim
negSubFactor = try unaArithExpr <|> subFactor

subFactor :: ParsecT [Token] State IO Token
subFactor = base >>= evalSubFactorRemaining

evalSubFactorRemaining :: Token -> ParsecT [Token] State IO Token
evalSubFactorRemaining n1 =
  ( do
      op <- powToken
      n2 <- base
      evalSubFactorRemaining (eval n1 op n2)
  )
    <|> return n1

base :: ParsecT [Token] State IO Token
base = bracketExpression <|> atomExpression

bracketExpression :: ParsecT [Token] State IO Token
bracketExpression = do
  l <- beginpToken
  expr <- expression
  r <- endpToken
  return expr

atomExpression :: ParsecT [Token] State IO Token
atomExpression = do
  n <- literalValueToken <|> idToken <|> convToFloat <|> convAbs
  evalVar n

-- Functions

convToFloat :: ParsecT [Token] State IO Token
convToFloat = do
  fun <- toFloatFun
  l <- beginpToken
  n <- expression
  r <- endpToken
  nEvaluated <- evalVar n
  case val nEvaluated of
    (I i) -> return $ LiteralValue (pos n) $ F (fromIntegral i)
    t -> error $ typeErrorUnary (pos n) "(toFloat)" t

convToStr :: ParsecT [Token] State IO Token
convToStr = do
  fun <- toStrFun
  l <- beginpToken
  n <- expression
  r <- endpToken
  return $ LiteralValue (pos n) (S $ show (val n))

convAbs :: ParsecT [Token] State IO Token
convAbs = do
  fun <- absFun
  l <- beginpToken
  n <- expression
  r <- endpToken
  return $ LiteralValue (pos n) (to_abs (pos n) $ val n)
  where
    to_abs p (I i) = I (abs i)
    to_abs p (F f) = F (abs f)
    to_abs p t = error $ typeErrorUnary p "(abs)" t

negValue :: Token -> Token
negValue (LiteralValue p a) =
  case a of
    B b -> LiteralValue p (B $ not b)
    I i -> LiteralValue p (I (-i))
    F f -> LiteralValue p (F (-f))

unaBoolExpr :: ParsecT [Token] State IO Token
unaBoolExpr = do
  op <- notToken
  b <- expression

  state@(_, _, (act_name, _) : stack, _, _) <- getState

  case b of
    LiteralValue p (B b) -> return $ LiteralValue p (B $ not b)
    Id p i -> return $ negValue $ symTableGetVal (scopeNameVar act_name i) p state

unaArithExpr :: ParsecT [Token] State IO Token
unaArithExpr = do
  op <- minusToken
  n1 <- subExpression

  state@(_, _, (act_name, _) : stack, _, _) <- getState

  case n1 of
    LiteralValue p (I i) -> return $ LiteralValue p (I (-i))
    LiteralValue p (F f) -> return $ LiteralValue p (F (-f))
    Id p i -> return $ negValue $ symTableGetVal (scopeNameVar act_name i) p state
