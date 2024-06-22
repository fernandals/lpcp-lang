module ExpressionsEvaluation where

import Control.Monad.IO.Class (liftIO)
import Errors
import Lexer
import State
import Text.Parsec hiding (State)
import Utils

evalVar :: Token -> ParsecT [Token] State IO Token
evalVar (Id p id) = do
  state@(_, _, (act_name, _) : stack, _, _) <- getState
  return $ symTableGetVal (scopeNameVar act_name id) p state
evalVar tk = return tk

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
    (I n, I m) -> B $ n <= m
    (F x, F y) -> B $ x <= y
    (C c, C d) -> B $ c <= d
    (S s, S u) -> B $ s <= u
    (a, b) -> error $ typeErrorRelation p "(<=)" a b
eval (LiteralValue p a) (Geq _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (I n, I m) -> B $ n >= m
    (F x, F y) -> B $ x >= y
    (C c, C d) -> B $ c >= d
    (S s, S u) -> B $ s >= u
    (a, b) -> error $ typeErrorRelation p "(>=)" a b
eval (LiteralValue p a) (Less _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (I n, I m) -> B $ n < m
    (F x, F y) -> B $ x < y
    (C c, C d) -> B $ c < d
    (S s, S u) -> B $ s < u
    (a, b) -> error $ typeErrorRelation p "(<)" a b
eval (LiteralValue p a) (Greater _) (LiteralValue p' b) = LiteralValue p $
  case (a, b) of
    (I n, I m) -> B $ n > m
    (F x, F y) -> B $ x > y
    (C c, C d) -> B $ c > d
    (S s, S u) -> B $ s > u
    (a, b) -> error $ typeErrorRelation p "(>)" a b
