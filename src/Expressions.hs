{-# LANGUAGE UnicodeSyntax #-}

module Expressions where

import Builtin
import Control.Monad.IO.Class (liftIO)
import Data.IntMap (update)
import Lexer
import Lexer (Token (LiteralValue))
import State
import Text.Parsec hiding (State)
import Tokens
import Utils

expression :: ParsecT [Token] State IO Token -- precisa melhoria nessa ordem, pq essa foi escolhida na tentativa e erro
expression = do
  n1 <- term
  evalExpressionRemaining n1

evalExpressionRemaining :: Token -> ParsecT [Token] State IO Token
evalExpressionRemaining n1 =
  ( do
      op <- orToken <|> xorToken
      n2 <- term
      evalExpressionRemaining $ eval n1 op n2
  )
    <|> return n1

term :: ParsecT [Token] State IO Token
term = do
  n1 <- notFactor
  evalTermRemaining n1

evalTermRemaining :: Token -> ParsecT [Token] State IO Token
evalTermRemaining n1 =
  ( do
      op <- andToken
      n2 <- notFactor
      evalTermRemaining $ eval n1 op n2
  )
    <|> return n1

notFactor :: ParsecT [Token] State IO Token -- pode dar ruim
notFactor = try unaBoolExpr <|> factor

factor :: ParsecT [Token] State IO Token
factor = try relation <|> try bracket <|> subExpression

relation :: ParsecT [Token] State IO Token
relation = do
  n1 <- try bracket <|> subExpression
  rel <- leqToken <|> geqToken <|> lessToken <|> greaterToken <|> eqToken <|> neqToken
  n2 <- try bracket <|> subExpression
  return $ eval n1 rel n2

subExpression :: ParsecT [Token] State IO Token
subExpression = do
  n1 <- subTerm
  evalSubExpressionRemaining n1

evalSubExpressionRemaining :: Token -> ParsecT [Token] State IO Token
evalSubExpressionRemaining n1 =
  ( do
      op <- plusToken <|> minusToken
      n2 <- subTerm
      evalSubExpressionRemaining (eval n1 op n2)
  )
    <|> return n1

subTerm :: ParsecT [Token] State IO Token
subTerm = do
  n1 <- negSubFactor
  evalSubTermRemaining n1

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
subFactor = do
  n1 <- base
  evalSubFactorRemaining n1

evalSubFactorRemaining :: Token -> ParsecT [Token] State IO Token
evalSubFactorRemaining n1 =
  ( do
      op <- powToken
      n2 <- base
      evalSubFactorRemaining (eval n1 op n2)
  )
    <|> return n1

base :: ParsecT [Token] State IO Token
base = subBracket <|> atomExpression

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

-- GAMBIARRA UNARIAS & BRACKETs

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

evalVar :: Token -> ParsecT [Token] State IO Token
evalVar (Id p id) = do
  state@(_, _, (act_name, _) : stack, _, _) <- getState

  return $ symTableGetVal (scopeNameVar act_name id) p state
evalVar token = return token

negValue :: Token -> Token
negValue (LiteralValue p a) =
  case a of
    B b -> LiteralValue p (B $ not b)
    I i -> LiteralValue p (I (-i))
    F f -> LiteralValue p (F (-f))

bracket :: ParsecT [Token] State IO Token
bracket = do
  l <- beginpToken
  expr <- expression
  r <- endpToken
  return expr

subBracket :: ParsecT [Token] State IO Token
subBracket = do
  l <- beginpToken
  expr <- subExpression
  r <- endpToken
  return expr

-- EVAL

eval :: Token -> Token -> Token -> Token
eval (LiteralValue p a) (Plus _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (I n, I m) -> I $ n + m
    (F x, F y) -> F $ x + y
    (a, b) -> error $ typeErrorExprMsg p "(+)" a b
eval (LiteralValue p a) (Minus _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (I n, I m) -> I $ n - m
    (F x, F y) -> F $ x - y
    (a, b) -> error $ typeErrorExprMsg p "(-)" a b
eval (LiteralValue p a) (Times _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (I n, I m) -> I $ n * m
    (F x, F y) -> F $ x * y
    (a, b) -> error $ typeErrorExprMsg p "(*)" a b
eval (LiteralValue p a) (Divides _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (I n, I m) -> I $ n `div` m
    (F x, F y) -> F $ x / y
    (a, b) -> error $ typeErrorExprMsg p "(/)" a b
eval (LiteralValue p a) (Pow _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (I n, I m) ->
      if m >= 0
        then I $ n ^ m
        else
          error $
            "Error at evaluation of expression in "
              ++ show p
              ++ ".\nFound negative exponent with an integer base.\n"
    (F x, I y) ->
      if y >= 0
        then F $ x ^ y
        else F $ 1 / (x ^ (-y))
    (a, b) -> error $ typeErrorExprMsg p "(^)" a b
eval (LiteralValue p a) (Modulos _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (I n, I m) -> I $ mod n m
    (a, b) -> error $ typeErrorExprMsg p "(%)" a b
eval (LiteralValue p a) (And _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (B p, B q) -> B $ p && q
    (a, b) -> error $ typeErrorExprMsg p "(and)" a b
eval (LiteralValue p a) (Or _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (B p, B q) -> B $ p || q
    (a, b) -> error $ typeErrorExprMsg p "(or)" a b
eval (LiteralValue p a) (Xor _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (B p, B q) -> B $ (not p && q) || (p && not q)
    (a, b) -> error $ typeErrorExprMsg p "(xor)" a b
eval (LiteralValue p a) (Eq _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (B p, B q) -> B $ p == q
    (I n, I m) -> B $ n == m
    (F x, F y) -> B $ x == y
    (C c, C d) -> B $ c == d
    (S s, S u) -> B $ s == u
    (a, b) -> error $ typeErrorRelation p "(==)" a b
eval (LiteralValue p a) (Neq _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (B p, B q) -> B $ p /= q
    (I n, I m) -> B $ n /= m
    (F x, F y) -> B $ x /= y
    (C c, C d) -> B $ c /= d
    (S s, S u) -> B $ s /= u
    (a, b) -> error $ typeErrorRelation p "(!=)" a b
eval (LiteralValue p a) (Leq _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (B p, B q) -> B $ p <= q
    (I n, I m) -> B $ n <= m
    (F x, F y) -> B $ x <= y
    (C c, C d) -> B $ c <= d
    (S s, S u) -> B $ s <= u
    (a, b) -> error $ typeErrorRelation p "(<=)" a b
eval (LiteralValue p a) (Geq _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (B p, B q) -> B $ p >= q
    (I n, I m) -> B $ n >= m
    (F x, F y) -> B $ x >= y
    (C c, C d) -> B $ c >= d
    (S s, S u) -> B $ s >= u
    (a, b) -> error $ typeErrorRelation p "(>=)" a b
eval (LiteralValue p a) (Less _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (B p, B q) -> B $ p < q
    (I n, I m) -> B $ n < m
    (F x, F y) -> B $ x < y
    (C c, C d) -> B $ c < d
    (S s, S u) -> B $ s < u
    (a, b) -> error $ typeErrorRelation p "(<)" a b
eval (LiteralValue p a) (Greater _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (B p, B q) -> B $ p > q
    (I n, I m) -> B $ n > m
    (F x, F y) -> B $ x > y
    (C c, C d) -> B $ c > d
    (S s, S u) -> B $ s > u
    (a, b) -> error $ typeErrorRelation p "(>)" a b
