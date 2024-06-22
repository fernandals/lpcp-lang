{-# LANGUAGE UnicodeSyntax #-}

module ParserExpressions where

import Builtin
import Control.Monad.IO.Class (liftIO)
import Data.IntMap (update)
import Lexer
import Lexer (Token (LiteralValue))
import State
import Text.Parsec hiding (State)
import Tokens
import Utils


binExpr:: ParsecT [Token] State IO [Token] -- precisa melhoria nessa ordem, pq essa foi escolhida na tentativa e erro
binExpr = do
  n1 <- atom <|> unaExpr <|> bracktExpr
  binExprRemaining n1

binExprRemaining :: [Token] -> ParsecT [Token] State IO [Token]
binExprRemaining n1 =
  (do
      op <- binOp
      n2 <- binExpr
      return (n1 ++ [op] ++ n2)
  )
  <|> return n1

unaExpr :: ParsecT [Token] State IO [Token]
unaExpr = do
  op <- minusToken <|> notToken
  n1 <- atom
  return ( op : n1)

binOp ::  ParsecT [Token] State IO Token
binOp = do
  op <- plusToken
   <|> minusToken
   <|> timesToken
   <|> dividesToken
   <|> modulosToken
   <|> powToken
   <|> eqToken
   <|> neqToken
   <|> leqToken
   <|> geqToken
   <|> greaterToken
   <|> lessToken
   <|> orToken 
   <|> xorToken 
   <|> andToken
  return op

atom ::  ParsecT [Token] State IO [Token]
atom = do
  a <- idToken
    <|>  literalValueToken 

  return [a]

bracktExpr :: ParsecT [Token] State IO [Token]
bracktExpr = do
  l <- beginpToken
  exp <- try binExpr <|> unaExpr
  r <- endpToken
  return ([l]++exp++[r])

-- embaixo provavelmente eh lixo, mas nao apaguem ainda

syntaxChecker :: [Token] -> Token -> [Token] -> [Token]
syntaxChecker [(LiteralValue p a)] (Plus u) [(LiteralValue p' b)] =
  case (a, b) of
    (I n, I m) -> [(LiteralValue p a), (Plus u),(LiteralValue p' b)]
    (F x, F y) -> [(LiteralValue p a), (Plus u),(LiteralValue p' b)]
    (a, b) -> error $ typeErrorExprMsg p "(+)" a b
syntaxChecker [(LiteralValue p a)] (Minus u) [(LiteralValue p' b)] =
  case (a, b) of
    (I n, I m) ->  [(LiteralValue p a), (Minus u),(LiteralValue p' b)]
    (F x, F y) ->  [(LiteralValue p a), (Minus u),(LiteralValue p' b)]
    (a, b) -> error $ typeErrorExprMsg p "(-)" a b
syntaxChecker [(LiteralValue p a)] (Times u) [(LiteralValue p' b)] = 
  case (a, b) of
    (I n, I m) ->  [(LiteralValue p a), (Times u),(LiteralValue p' b)]
    (F x, F y) ->  [(LiteralValue p a), (Times u),(LiteralValue p' b)]
    (a, b) -> error $ typeErrorExprMsg p "(*)" a b
syntaxChecker [(LiteralValue p a)] (Divides u) [(LiteralValue p' b)] = 
  case (a, b) of
    (I n, I m) ->  [(LiteralValue p a), (Divides u),(LiteralValue p' b)]
    (F x, F y) ->  [(LiteralValue p a), (Divides u),(LiteralValue p' b)]
    (a, b) -> error $ typeErrorExprMsg p "(/)" a b
syntaxChecker [(LiteralValue p a)] (Pow u) [(LiteralValue p' b)] = 
  case (a, b) of
    (I n, I m) ->
      if m >= 0
        then [(LiteralValue p a), (Pow u),(LiteralValue p' b)]
        else
          error $
            "Error at evaluation of expression in "
              ++ show p
              ++ ".\nFound negative exponent with an integer base.\n"
    (F x, I y) ->
      if y >= 0
        then [(LiteralValue p a), (Pow u),(LiteralValue p' b)]
        else [(LiteralValue p a), (Pow u),(LiteralValue p' b)]
    (a, b) -> error $ typeErrorExprMsg p "(^)" a b
syntaxChecker [(LiteralValue p a)] (Modulos u) [(LiteralValue p' b)] =
  case (a, b) of
    (I n, I m) ->  [(LiteralValue p a), (Modulos u),(LiteralValue p' b)]
    (a, b) -> error $ typeErrorExprMsg p "(%)" a b
syntaxChecker [(LiteralValue p a)] (And u) [(LiteralValue p' b)] = 
  case (a, b) of
    (B x, B y) ->[(LiteralValue p a), (And u),(LiteralValue p' b)]
    (a, b) -> error $ typeErrorExprMsg p "(and)" a b
syntaxChecker [(LiteralValue p a)] (Or u) [(LiteralValue p' b)] =
  case (a, b) of
    (B x, B y) -> [(LiteralValue p a), (Or u),(LiteralValue p' b)]
    (a, b) -> error $ typeErrorExprMsg p "(or)" a b
syntaxChecker [(LiteralValue p a)] (Xor u) [(LiteralValue p' b)] =
  case (a, b) of
    (B x, B y) -> [(LiteralValue p a), (Xor u),(LiteralValue p' b)]
    (a, b) -> error $ typeErrorExprMsg p "(xor)" a b
syntaxChecker [(LiteralValue p a)] (Eq u) [(LiteralValue p' b)] = 
  case (a, b) of
    (B x, B y) -> [(LiteralValue p a), (Eq u), (LiteralValue p' b)]
    (I n, I m) -> [(LiteralValue p a), (Eq u), (LiteralValue p' b)]
    (F x, F y) -> [(LiteralValue p a), (Eq u), (LiteralValue p' b)]
    (C c, C d) -> [(LiteralValue p a), (Eq u), (LiteralValue p' b)]
    (S s, S z) -> [(LiteralValue p a), (Eq u), (LiteralValue p' b)]
    (a, b) -> error $ typeErrorRelation p "(==)" a b
syntaxChecker [(LiteralValue p a)] (Neq u) [(LiteralValue p' b)] = 
  case (a, b) of
    (B x, B y) -> [(LiteralValue p a), (Neq u), (LiteralValue p' b)]
    (I n, I m) -> [(LiteralValue p a), (Neq u), (LiteralValue p' b)]
    (F x, F y) -> [(LiteralValue p a), (Neq u), (LiteralValue p' b)]
    (C c, C d) -> [(LiteralValue p a), (Neq u), (LiteralValue p' b)]
    (S s, S z) -> [(LiteralValue p a), (Neq u), (LiteralValue p' b)]
    (a, b) -> error $ typeErrorRelation p "(!=)" a b
syntaxChecker [(LiteralValue p a)] (Leq u) [(LiteralValue p' b)] = 
  case (a, b) of
    (B x, B y) -> [(LiteralValue p a), (Leq u), (LiteralValue p' b)]
    (I n, I m) -> [(LiteralValue p a), (Leq u), (LiteralValue p' b)]
    (F x, F y) -> [(LiteralValue p a), (Leq u), (LiteralValue p' b)]
    (C c, C d) -> [(LiteralValue p a), (Leq u), (LiteralValue p' b)]
    (S s, S z) -> [(LiteralValue p a), (Leq u), (LiteralValue p' b)]
    (a, b) -> error $ typeErrorRelation p "(<=)" a b
syntaxChecker [(LiteralValue p a)] (Geq u) [(LiteralValue p' b)] =
  case (a, b) of
    (B x, B y) -> [(LiteralValue p a), (Geq u), (LiteralValue p' b)]
    (I n, I m) -> [(LiteralValue p a), (Geq u), (LiteralValue p' b)]
    (F x, F y) -> [(LiteralValue p a), (Geq u), (LiteralValue p' b)]
    (C c, C d) -> [(LiteralValue p a), (Geq u), (LiteralValue p' b)]
    (S s, S z) -> [(LiteralValue p a), (Geq u), (LiteralValue p' b)]
    (a, b) -> error $ typeErrorRelation p "(>=)" a b
syntaxChecker [(LiteralValue p a)] (Less u) [(LiteralValue p' b)] = 
  case (a, b) of
    (B x, B y) -> [(LiteralValue p a), (Less u), (LiteralValue p' b)]
    (I n, I m) -> [(LiteralValue p a), (Less u), (LiteralValue p' b)]
    (F x, F y) -> [(LiteralValue p a), (Less u), (LiteralValue p' b)]
    (C c, C d) -> [(LiteralValue p a), (Less u), (LiteralValue p' b)]
    (S s, S z) -> [(LiteralValue p a), (Less u), (LiteralValue p' b)]
    (a, b) -> error $ typeErrorRelation p "(<)" a b
syntaxChecker [(LiteralValue p a)] (Greater u) [(LiteralValue p' b)] = 
  case (a, b) of
    (B x, B y) -> [(LiteralValue p a), (Greater u), (LiteralValue p' b)]
    (I n, I m) -> [(LiteralValue p a), (Greater u), (LiteralValue p' b)]
    (F x, F y) -> [(LiteralValue p a), (Greater u), (LiteralValue p' b)]
    (C c, C d) -> [(LiteralValue p a), (Greater u), (LiteralValue p' b)]
    (S s, S z) -> [(LiteralValue p a), (Greater u), (LiteralValue p' b)]
    (a, b) -> error $ typeErrorRelation p "(>)" a b
