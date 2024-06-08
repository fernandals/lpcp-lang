module Arith where

import Control.Monad.IO.Class (liftIO)
import Data.IntMap (update)
import Lexer
import State
import Text.Parsec hiding (State)
import Tokens

type ParsecTStateT = ParsecT [Token] State IO(Token)

atomExpr :: ParsecT [Token] State IO(Token)
atomExpr = do 
   n <- intLToken 
        <|> floatLToken 
        <|> idToken 
        <|> convToFloat 
        <|> convToStr 
        <|> convAbs
   evalVar n

evalVar :: Token -> ParsecT [Token] State IO Token
evalVar (Id p id) = do
    σ <- getState
    return $ getValue (Id p id) σ
evalVar token = return token

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
   n1 <- try bracketExpr <|> atomExpr
   result <- evalPowRemaining n1
   return (result)

evalPowRemaining :: Token -> ParsecT [Token] State IO(Token)
evalPowRemaining n1 = do
  op <- powToken
  n2 <- try bracketExpr <|>  atomExpr
  result <- evalPowRemaining (evalArith n1 op n2)
  return (result) 
  <|> return (n1) 

bracketExpr :: ParsecT [Token] State IO(Token)
bracketExpr = do
  l <- beginpToken
  expr <- binArithExpr
  r <- endpToken
  return (expr)

evalRemaining :: Token -> ParsecT [Token] State IO(Token)
evalRemaining n1 = do
  op <- powToken
  n2 <- atomExpr
  result <- evalRemaining(evalArith n1 op n2)
  return (result) 
  <|> return (n1) 

evalArith :: Token -> Token -> Token -> Token
evalArith (IntL p x) (Plus _) (IntL r y) = IntL p (x + y)
evalArith (IntL p x) (Minus _) (IntL r y) =  IntL p (x - y)
evalArith (IntL p x) (Times _) (IntL r y) =  IntL p (x * y)
evalArith (IntL p x) (Divides _) (IntL r y) =  IntL p (x `div` y)
evalArith (IntL p x) (Pow _) (IntL r y) =  IntL p (x ^ y)

-- INT: functions

convToFloat :: ParsecT [Token] State IO(Token)
convToFloat = do 
  fun <- toFloatToken
  l <- beginpToken
  n <- intLToken
  r <- endpToken
  return (FloatL (pInt n) (fromIntegral (valueInt n)))

convToStr :: ParsecT [Token] State IO(Token)
convToStr = do 
  fun <- toStrToken
  l <- beginpToken
  n <- intLToken
  r <- endpToken
  return (StringL (pInt n) (show (valueInt n)))

convAbs :: ParsecT [Token] State IO(Token)
convAbs = do 
  fun <- absToken
  l <- beginpToken
  n <- intLToken
  r <- endpToken
  return (IntL (pInt n) (abs (valueInt n)))

valueInt :: Token -> Integer
valueInt (IntL p n) = n 

pInt :: Token -> (Int,Int)
pInt (IntL p n) = p 
