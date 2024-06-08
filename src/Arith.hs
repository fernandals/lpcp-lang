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
        <|> convAbs
        <|> unaOnBinArithExpr
   evalVar n

unaOnBinArithExpr :: ParsecT [Token] State IO(Token) 
unaOnBinArithExpr = do
    l <- beginpToken
    op <- minusToken
    n1 <- intLToken <|> idToken -- em caso de -id precisa pensar ainda
    r <- beginpToken
    return (n1)

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

unaArithExpr :: ParsecT [Token] State IO(Token) 
unaArithExpr = do
   op <- minusToken
   n1 <- intLToken <|> idToken -- em caso de -id precisa pensar ainda
   return (n1)

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


-- Ajeitar isso depois pra nao ficar duplicando onde nao precisar ( + , - , * )

evalArith :: Token -> Token -> Token -> Token
evalArith (IntL p x) (Plus _) (IntL r y) = IntL p (x + y)
evalArith (IntL p x) (Minus _) (IntL r y) =  IntL p (x - y)
evalArith (IntL p x) (Times _) (IntL r y) =  IntL p (x * y)
evalArith (IntL p x) (Divides _) (IntL r y) =  IntL p (x `div` y)
evalArith (IntL p x) (Pow _) (IntL r y) = if y >= 0 then IntL p (x ^ y) else error "Type mismatch: change to float"
evalArith (FloatL p x) (Plus _) (FloatL r y) = FloatL p (x + y)
evalArith (FloatL p x) (Minus _) (FloatL r y) =  FloatL p (x - y)
evalArith (FloatL p x) (Times _) (FloatL r y) =  FloatL p (x * y)
evalArith (FloatL p x) (Divides _) (FloatL r y) =  FloatL p (x / y)
evalArith (FloatL p x) (Pow _) (IntL r y) = if y >= 0 then FloatL p (x ^ y) else  FloatL p (1 / (x ^ (-y)))
evalArith _ _ _ = error "Type mismatch"

-- INT: functions

convToFloat :: ParsecT [Token] State IO(Token)
convToFloat = do 
  fun <- toFloatToken
  l <- beginpToken
  n <- intLToken <|> idToken <|> binArithExpr
  r <- endpToken
  nEvaluated <- evalVar n
  case nEvaluated of
    IntL p i -> return (FloatL p (fromIntegral i))
    _ -> fail "Expected an integer token"

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
valueInt _ = error "Not an integer token"

pInt :: Token -> (Int,Int)
pInt (IntL p n) = p 
