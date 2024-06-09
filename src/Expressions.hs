module Expressions where

import Control.Monad.IO.Class (liftIO)
import Data.IntMap (update)
import Lexer
import State
import Text.Parsec hiding (State)
import Tokens

-- ARITH

atomExpr :: ParsecT [Token] State IO(Token)
atomExpr = do 
    n <- intLToken 
        <|> floatLToken 
        <|> idToken 
        <|> convToFloat 
        <|> convAbs
    evalVar n

evalVar :: Token -> ParsecT [Token] State IO Token
evalVar (Id p id) = do
    σ <- getState
    return $ getValue (Id p id) σ
evalVar token = return token

unaArithExpr :: ParsecT [Token] State IO(Token) 
unaArithExpr = do
   op <- minusToken
   n1 <- intLToken <|> floatLToken <|> idToken  <|> numberRelation
   σ <- getState
   case n1 of
    IntL p i -> return (IntL p (-i))
    FloatL p i ->  return (FloatL p (-i))
    Id p i -> return (negValue (getValue (Id p i) σ))
    _ -> fail "Expected an number token"

negValue :: Token -> Token
negValue (IntL p n) = (IntL p (-n))
negValue (FloatL p n) = (FloatL p (-n))
negValue _ = error "is not a value"

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
    expr <- binArithExpr <|> unaArithExpr
    r <- endpToken
    return (expr)

evalRemaining :: Token -> ParsecT [Token] State IO(Token) --nem lembro pra que tinha feito essa, analisar se eh removivel agora
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

-- Int functions

convToFloat :: ParsecT [Token] State IO(Token)
convToFloat = do 
  fun <- toFloatToken
  l <- beginpToken
  n <- binArithExpr <|> unaArithExpr
  r <- endpToken
  nEvaluated <- evalVar n
  case nEvaluated of
    IntL p i -> return (FloatL p (fromIntegral i))
    _ -> fail "Expected an integer token"

convToStr :: ParsecT [Token] State IO(Token)
convToStr = do 
  fun <- toStrToken
  l <- beginpToken
  n <- binArithExpr <|> unaArithExpr
  r <- endpToken
  return (StringL (pos n) (show (valueInt n)))

convAbs :: ParsecT [Token] State IO(Token)
convAbs = do 
  fun <- absToken
  l <- beginpToken
  n <- binArithExpr <|> unaArithExpr
  r <- endpToken
  return (IntL (pos n) (abs (valueInt n)))


valueInt :: Token -> Integer
valueInt (IntL p n) = n
valueInt _ = error "Not an integer token"

valueFloat :: Token -> Float
valueFloat (FloatL p n) = n
valueFloat _ = error "Not an float token"

pos :: Token -> (Int,Int)
pos (IntL p n) = p 


-- BOOL


atomBoolExpr :: ParsecT [Token] State IO(Token)
atomBoolExpr = do 
    b <- boolLToken <|> idToken
    return b

unaBoolExpr :: ParsecT [Token] State IO(Token) 
unaBoolExpr= do
   op <- notToken
   b <- boolLToken <|> idToken <|> bracketBoolExpr --resolver para id
   σ <- getState
   case b of
    BoolL p i -> return (BoolL p (not i))
    Id p i -> return (negBoolValue (getValue (Id p i) σ))
    _ -> fail "Expected an number token"

negBoolValue :: Token -> Token
negBoolValue (BoolL p b) = (BoolL p (not b))
negBoolValue _ = error "is not a value"

binBoolExpr :: ParsecT [Token] State IO(Token)
binBoolExpr = do
   b <-  termBoolExpr
   result <- evalBoolRemaining b
   return (result)

evalBoolRemaining :: Token -> ParsecT [Token] State IO(Token)
evalBoolRemaining b = do
    op <- orToken <|> xorToken
    d <- termBoolExpr
    result <- evalBoolRemaining (evalBool b op d)
    return (result) 
    <|> return (b) 

termBoolExpr :: ParsecT [Token] State IO(Token)
termBoolExpr = do
    b <- factorBoolExpr
    result <- evalTermBoolRemaining b
    return (result)

evalTermBoolRemaining :: Token -> ParsecT [Token] State IO(Token)
evalTermBoolRemaining b = do
    op <- andToken
    d <- factorBoolExpr
    result <- evalTermBoolRemaining (evalBool b op d)
    return (result) 
    <|> return (b)

factorBoolExpr :: ParsecT [Token] State IO(Token)
factorBoolExpr = do
    n1 <- bracketBoolExpr <|> unaBoolExpr <|> atomBoolExpr <|> numberRelation
    return (n1)


bracketBoolExpr :: ParsecT [Token] State IO(Token)
bracketBoolExpr = do
    l <- beginpToken
    expr <- binBoolExpr <|> unaBoolExpr
    r <- endpToken
    return (expr)

    
evalBool :: Token -> Token -> Token -> Token
evalBool (BoolL p x) (And _) (BoolL r y) = BoolL p (x &&  y)
evalBool (BoolL p x) (Or _) (BoolL r y) = BoolL p (x || y)
evalBool (BoolL p x) (Xor _) (BoolL r y) = BoolL p (((not x) &&  y) || (x && (not y)) )


-- NUMBER : RELATIONS 

numberRelation :: ParsecT [Token] State IO(Token) -- problema: nao to conseguindo fazer sem parenteses em volta
numberRelation = do 
    n1 <- try binArithExpr <|> unaArithExpr
    rel <- leqToken <|> geqToken
    n2 <- try binArithExpr <|> unaArithExpr
    return (evalRel n1 rel n2)

evalRel :: Token -> Token -> Token  -> Token
evalRel (IntL p x) (Leq r) (IntL q y) = (BoolL p (x <= y))
evalRel (IntL p x) (Geq r) (IntL q y) = (BoolL p (x >= y))
