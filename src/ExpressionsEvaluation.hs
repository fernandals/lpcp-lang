{-# LANGUAGE TupleSections #-}

module ExpressionsEvaluation where

import Control.Monad.IO.Class (liftIO)
import Errors
import Lexer
import State
import Text.Parsec hiding (State)
import Utils

evalVar :: (Token, [Token]) -> ParsecT [Token] State IO (Token, [Token])
evalVar (Id p id, expr) = do
  state@(_, _, (act_name, _) : _, _, _) <- getState
  return (symTableGetVal (scopeNameVar act_name id) p state, expr)
evalVar tk = return tk

eval :: (Token, [Token]) -> Token -> (Token, [Token]) -> (Token, [Token])
eval (LiteralValue p a, expr) (Plus op_p) (LiteralValue p' b, expr') = (,expr ++ Plus op_p : expr') $
  LiteralValue p $
    case (a, b) of
      (I n, I m) -> I $ n + m
      (F x, F y) -> F $ x + y
      (a, b) -> error $ typeErrorExprMsg p "(+)" a b
eval (LiteralValue p a, expr) (Minus op_p) (LiteralValue p' b, expr') = (,expr ++ Minus op_p : expr') $
  LiteralValue p $
    case (a, b) of
      (I n, I m) -> I $ n - m
      (F x, F y) -> F $ x - y
      (a, b) -> error $ typeErrorExprMsg p "(-)" a b
eval (LiteralValue p a, expr) (Times op_p) (LiteralValue p' b, expr') = (,expr ++ Times op_p : expr') $
  LiteralValue p $
    case (a, b) of
      (I n, I m) -> I $ n * m
      (F x, F y) -> F $ x * y
      (a, b) -> error $ typeErrorExprMsg p "(*)" a b
eval (LiteralValue p a, expr) (Divides op_p) (LiteralValue p' b, expr') = (,expr ++ Divides op_p : expr') $
  LiteralValue p $
    case (a, b) of
      (I n, I m) -> I $ n `div` m
      (F x, F y) -> F $ x / y
      (a, b) -> error $ typeErrorExprMsg p "(/)" a b
eval (LiteralValue p a, expr) (Pow op_p) (LiteralValue p' b, expr') = (,expr ++ Pow op_p : expr') $
  LiteralValue p $
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
eval (LiteralValue p a, expr) (Modulos op_p) (LiteralValue p' b, expr') = (,expr ++ Modulos op_p : expr') $
  LiteralValue p $
    case (a, b) of
      (I n, I m) -> I $ mod n m
      (a, b) -> error $ typeErrorExprMsg p "(%)" a b
eval (LiteralValue p a, expr) (And op_p) (LiteralValue p' b, expr') = (,expr ++ And op_p : expr') $
  LiteralValue p $
    case (a, b) of
      (B p, B q) -> B $ p && q
      (a, b) -> error $ typeErrorExprMsg p "(and)" a b
eval (LiteralValue p a, expr) (Or op_p) (LiteralValue p' b, expr') = (,expr ++ Or op_p : expr') $
  LiteralValue p $
    case (a, b) of
      (B p, B q) -> B $ p || q
      (a, b) -> error $ typeErrorExprMsg p "(or)" a b
eval (LiteralValue p a, expr) (Xor op_p) (LiteralValue p' b, expr') = (,expr ++ Xor op_p : expr') $
  LiteralValue p $
    case (a, b) of
      (B p, B q) -> B $ (not p && q) || (p && not q)
      (a, b) -> error $ typeErrorExprMsg p "(xor)" a b
eval (LiteralValue p a, expr) (Eq op_p) (LiteralValue p' b, expr') = (,expr ++ Eq op_p : expr') $
  LiteralValue p $
    case (a, b) of
      (B p, B q) -> B $ p == q
      (I n, I m) -> B $ n == m
      (F x, F y) -> B $ x == y
      (C c, C d) -> B $ c == d
      (S s, S u) -> B $ s == u
      (a, b) -> error $ typeErrorRelation p "(==)" a b
eval (LiteralValue p a, expr) (Neq op_p) (LiteralValue p' b, expr') = (,expr ++ Neq op_p : expr') $
  LiteralValue p $
    case (a, b) of
      (B p, B q) -> B $ p /= q
      (I n, I m) -> B $ n /= m
      (F x, F y) -> B $ x /= y
      (C c, C d) -> B $ c /= d
      (S s, S u) -> B $ s /= u
      (a, b) -> error $ typeErrorRelation p "(!=)" a b
eval (LiteralValue p a, expr) (Leq op_p) (LiteralValue p' b, expr') = (,expr ++ Leq op_p : expr') $
  LiteralValue p $
    case (a, b) of
      (I n, I m) -> B $ n <= m
      (F x, F y) -> B $ x <= y
      (C c, C d) -> B $ c <= d
      (S s, S u) -> B $ s <= u
      (a, b) -> error $ typeErrorRelation p "(<=)" a b
eval (LiteralValue p a, expr) (Geq op_p) (LiteralValue p' b, expr') = (,expr ++ Geq op_p : expr') $
  LiteralValue p $
    case (a, b) of
      (I n, I m) -> B $ n >= m
      (F x, F y) -> B $ x >= y
      (C c, C d) -> B $ c >= d
      (S s, S u) -> B $ s >= u
      (a, b) -> error $ typeErrorRelation p "(>=)" a b
eval (LiteralValue p a, expr) (Less op_p) (LiteralValue p' b, expr') = (,expr ++ Less op_p : expr') $
  LiteralValue p $
    case (a, b) of
      (I n, I m) -> B $ n < m
      (F x, F y) -> B $ x < y
      (C c, C d) -> B $ c < d
      (S s, S u) -> B $ s < u
      (a, b) -> error $ typeErrorRelation p "(<)" a b
eval (LiteralValue p a, expr) (Greater op_p) (LiteralValue p' b, expr') = (,expr ++ Greater op_p : expr') $
  LiteralValue p $
    case (a, b) of
      (I n, I m) -> B $ n > m
      (F x, F y) -> B $ x > y
      (C c, C d) -> B $ c > d
      (S s, S u) -> B $ s > u
      (a, b) -> error $ typeErrorRelation p "(>)" a b
