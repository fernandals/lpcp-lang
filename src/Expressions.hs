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

expression :: ParsecT [Token] State IO (Token, [Token])
expression = term >>= expressionRemaining

expressionRemaining :: (Token, [Token]) -> ParsecT [Token] State IO (Token, [Token])
expressionRemaining n1 =
  ( do
      op <- orToken <|> xorToken
      n2 <- term
      expressionRemaining $ eval n1 op n2
  )
    <|> return n1

term :: ParsecT [Token] State IO (Token, [Token])
term = notFactor >>= termRemaining

termRemaining :: (Token, [Token]) -> ParsecT [Token] State IO (Token, [Token])
termRemaining n1 =
  ( do
      op <- andToken
      n2 <- notFactor
      termRemaining $ eval n1 op n2
  )
    <|> return n1

notFactor :: ParsecT [Token] State IO (Token, [Token])
notFactor = try unaBoolExpr <|> factor

factor :: ParsecT [Token] State IO (Token, [Token])
factor = subExpression >>= factorRemaining

factorRemaining :: (Token, [Token]) -> ParsecT [Token] State IO (Token, [Token])
factorRemaining n1 =
  ( do
      rel <- leqToken <|> geqToken <|> lessToken <|> greaterToken <|> eqToken <|> neqToken
      n2 <- subExpression
      factorRemaining $ eval n1 rel n2
  )
    <|> return n1

subExpression :: ParsecT [Token] State IO (Token, [Token])
subExpression = subTerm >>= evalSubExpressionRemaining

evalSubExpressionRemaining :: (Token, [Token]) -> ParsecT [Token] State IO (Token, [Token])
evalSubExpressionRemaining n1 =
  ( do
      op <- plusToken <|> minusToken
      n2 <- subTerm
      evalSubExpressionRemaining (eval n1 op n2)
  )
    <|> return n1

subTerm :: ParsecT [Token] State IO (Token, [Token])
subTerm = negSubFactor >>= evalSubTermRemaining

evalSubTermRemaining :: (Token, [Token]) -> ParsecT [Token] State IO (Token, [Token])
evalSubTermRemaining n1 =
  ( do
      op <- timesToken <|> dividesToken <|> modulosToken
      n2 <- negSubFactor
      evalSubTermRemaining (eval n1 op n2)
  )
    <|> return n1

negSubFactor :: ParsecT [Token] State IO (Token, [Token]) -- pode dar ruim
negSubFactor = try unaArithExpr <|> subFactor

subFactor :: ParsecT [Token] State IO (Token, [Token])
subFactor = base >>= evalSubFactorRemaining

evalSubFactorRemaining :: (Token, [Token]) -> ParsecT [Token] State IO (Token, [Token])
evalSubFactorRemaining n1 =
  ( do
      op <- powToken
      n2 <- base
      evalSubFactorRemaining (eval n1 op n2)
  )
    <|> return n1

base :: ParsecT [Token] State IO (Token, [Token])
base = bracketExpression <|> atomExpression

bracketExpression :: ParsecT [Token] State IO (Token, [Token])
bracketExpression = do
  l <- beginpToken
  (v, expr) <- expression
  r <- endpToken
  return (v, l : expr ++ [r])

literalExpression :: ParsecT [Token] State IO (Token, [Token])
literalExpression = do
  n <- literalValueToken
  return (n, [n])

idExpression :: ParsecT [Token] State IO (Token, [Token])
idExpression = do
  n <- idToken
  return (n, [n])

atomExpression :: ParsecT [Token] State IO (Token, [Token])
atomExpression = do
  n <- literalExpression <|> idExpression <|> list <|> convToFloat <|> convAbs
  evalVar n

-- list expressions

list :: ParsecT [Token] State IO (Token, [Token])
list = do
  l <- beginSBToken
  elements <- expression `sepBy` commaToken
  r <- endSBToken
  listType <- tokensToTypes elements
  return $ (LiteralValue (pos l) (L (typeOfList listType (pos l)) 1 listType), l : concatMap snd elements ++ [r])

tokensToTypes :: [(Token, [Token])] -> ParsecT [Token] State IO [Type]
tokensToTypes [] = return []
tokensToTypes (x:xs) = do
  t <- tokenToType x
  ts <- tokensToTypes xs
  return (t:ts)

tokenToType :: (Token, [Token]) -> ParsecT [Token] State IO Type
tokenToType x = do
  case fst x of
    LiteralValue p (I i) -> return $ I i
    LiteralValue p (F f) -> return $ F f
    LiteralValue p (C c) -> return $ C c
    LiteralValue p (S s) -> return $ S s
    LiteralValue p (L t i l) -> return $ L t i l

typeOfList :: [Type] -> (Int,Int) -> Token
typeOfList [] _ = error $ "lista vazia ainda nao tratei"
typeOfList [x] p = typeOfx x p
typeOfList (x:y:xs) p = if (typeof' x) == (typeof' y)
  then typeOfList (y:xs) p
  else error $ "tipos heterogeneos"

typeOfx :: Type -> (Int,Int) -> Token
typeOfx x p = 
  case x of
    (I i) -> Int p
    (F f) -> Float p
    (C c) -> Char p
    (S s) -> String p
    (L t i _) -> List p t

-- Functions

convToFloat :: ParsecT [Token] State IO (Token, [Token])
convToFloat = do
  fun <- toFloatFun
  l <- beginpToken
  (v, expr) <- expression
  r <- endpToken
  (nEvaluated, expr) <- evalVar (v, expr)
  case val nEvaluated of
    (I i) -> return (LiteralValue (pos v) $ F (fromIntegral i), fun : l : expr ++ [r])
    t -> error $ typeErrorUnary (pos v) "(toFloat)" t

convToStr :: ParsecT [Token] State IO (Token, [Token])
convToStr = do
  fun <- toStrFun
  l <- beginpToken
  (v, expr) <- expression
  r <- endpToken
  return (LiteralValue (pos v) (S $ show (val v)), fun : l : expr ++ [r])

convAbs :: ParsecT [Token] State IO (Token, [Token])
convAbs = do
  fun <- absFun
  l <- beginpToken
  (v, expr) <- expression
  r <- endpToken
  return (LiteralValue (pos v) (to_abs (pos v) $ val v), fun : l : expr ++ [r])
  where
    to_abs p (I i) = I (abs i)
    to_abs p (F f) = F (abs f)
    to_abs p t = error $ typeErrorUnary p "(abs)" t

negValue :: (Token, [Token]) -> (Token, [Token])
negValue (LiteralValue p a, expr) =
  case a of
    B b -> (LiteralValue p (B $ not b), expr)
    I i -> (LiteralValue p (I (-i)), expr)
    F f -> (LiteralValue p (F (-f)), expr)

unaBoolExpr :: ParsecT [Token] State IO (Token, [Token])
unaBoolExpr = do
  op <- notToken
  b <- expression

  state@(_, _, (act_name, _) : stack, _, _) <- getState

  case b of
    (LiteralValue p (B b), expr) -> return (LiteralValue p (B $ not b), op : expr)
    (Id p i, expr) -> return $ negValue (symTableGetVal (scopeNameVar act_name i) p state, op : expr)

unaArithExpr :: ParsecT [Token] State IO (Token, [Token])
unaArithExpr = do
  op <- minusToken
  n1 <- subExpression

  state@(_, _, (act_name, _) : stack, _, _) <- getState

  case n1 of
    (LiteralValue p (I i), expr) -> return (LiteralValue p (I (-i)), op : expr)
    (LiteralValue p (F f), expr) -> return (LiteralValue p (F (-f)), op : expr)
    (Id p i, expr) -> return $ negValue (symTableGetVal (scopeNameVar act_name i) p state, op : expr)
