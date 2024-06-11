{-# LANGUAGE UnicodeSyntax #-} 

module Expressions where

import Control.Monad.IO.Class (liftIO)
import Data.IntMap (update)
import Lexer
import State
import Text.Parsec hiding (State)
import Tokens

expression :: ParsecT [Token] State IO(Token) -- precisa melhoria nessa ordem, pq essa foi escolhida na tentativa e erro
expression = do
    n1 <-  term
    result <- evalExpressionRemaining n1
    return (result)

evalExpressionRemaining :: Token -> ParsecT [Token] State IO(Token)
evalExpressionRemaining n1 = (do
        op <- orToken <|> xorToken
        n2 <- term
        result <- evalExpressionRemaining (eval n1 op n2)
        return (result))
    <|> return (n1)

term :: ParsecT [Token] State IO(Token)
term= do
    n1 <- notFactor
    result <- evalTermRemaining n1
    return (result)

evalTermRemaining :: Token -> ParsecT [Token] State IO(Token)
evalTermRemaining n1 = (do
        op <- andToken
        n2 <- notFactor
        result <- evalTermRemaining (eval n1 op n2)
        return (result))
    <|> return (n1)

notFactor :: ParsecT [Token] State IO(Token) --pode dar ruim
notFactor = do
    n1 <- try unaBoolExpr <|> factor
    return (n1)

unaBoolExpr :: ParsecT [Token] State IO(Token) 
unaBoolExpr= do
    op <- notToken
    b <-  expression
    σ <- getState
    case b of
        BoolL p i -> return (BoolL p (not i))
        Id p i -> return (negValue (getValue (Id p i) σ))
        _ -> fail "Expected an number token"

factor ::  ParsecT [Token] State IO(Token)
factor = do
    n1 <- try relation <|> try bracket <|> subExpression
    return (n1)


bracket :: ParsecT [Token] State IO(Token)
bracket = do
    l <- beginpToken
    expr <- expression
    r <- endpToken
    return (expr)

relation ::  ParsecT [Token] State IO(Token) -- problema: nao to conseguindo fazer sem parenteses em volta
relation = do 
    n1 <- subExpression
    rel <- leqToken <|> geqToken <|> lessToken <|> greaterToken <|> eqToken <|> neqToken
    n2 <- subExpression
    return (eval n1 rel n2)

subExpression :: ParsecT [Token] State IO(Token)
subExpression = do
   n1 <- subTerm
   result <- evalSubExpressionRemaining n1
   return (result)

evalSubExpressionRemaining :: Token -> ParsecT [Token] State IO(Token)
evalSubExpressionRemaining n1 = (do
        op <- plusToken <|> minusToken
        n2 <- subTerm
        result <- evalSubExpressionRemaining (eval n1 op n2)
        return (result))
    <|> return (n1) 

subTerm :: ParsecT [Token] State IO(Token)
subTerm = do
    n1 <- negSubFactor
    result <- evalSubTermRemaining n1
    return (result)

evalSubTermRemaining :: Token -> ParsecT [Token] State IO(Token)
evalSubTermRemaining n1 = (do
        op <- timesToken <|> dividesToken
        n2 <- negSubFactor
        result <- evalSubTermRemaining  (eval n1 op n2)
        return (result))
    <|> return (n1) 

negSubFactor ::  ParsecT [Token] State IO(Token) --pode dar ruim
negSubFactor  = do
    n1 <- try unaArithExpr <|> subFactor
    return (n1)

evalVar :: Token -> ParsecT [Token] State IO Token
evalVar (Id p id) = do
    σ <- getState
    return $ getValue (Id p id) σ
evalVar token = return token

unaArithExpr :: ParsecT [Token] State IO(Token) 
unaArithExpr = do
   op <- minusToken
   n1 <- subExpression
   σ <- getState
   case n1 of
    IntL p i -> return (IntL p (-i))
    FloatL p i ->  return (FloatL p (-i))
    Id p i -> return (negValue (getValue (Id p i) σ))
    _ -> fail "Expected an number token"

subFactor :: ParsecT [Token] State IO(Token)
subFactor = do
    n1 <- base
    result <- evalSubFactorRemaining n1
    return (result)

evalSubFactorRemaining :: Token -> ParsecT [Token] State IO(Token)
evalSubFactorRemaining n1 = (do
        op <- powToken
        n2 <- base
        result <- evalSubFactorRemaining (eval n1 op n2)
        return (result))
    <|> return (n1) 

base :: ParsecT [Token] State IO(Token)
base = do
    n1 <- subBracket <|> atomExpression
    return (n1)

subBracket :: ParsecT [Token] State IO(Token)
subBracket = do
    l <- beginpToken
    expr <- subExpression
    r <- endpToken
    return (expr)

atomExpression ::  ParsecT [Token] State IO(Token)
atomExpression = do 
    n <- intLToken 
        <|> floatLToken 
        <|> charLToken
        <|> boolLToken
        <|> idToken
        <|> convToFloat 
        <|> convAbs
    evalVar n


-- Functions

convToFloat :: ParsecT [Token] State IO(Token)
convToFloat = do 
  fun <- toFloatToken
  l <- beginpToken
  n <- expression
  r <- endpToken
  nEvaluated <- evalVar n
  case nEvaluated of
    IntL p i -> return (FloatL p (fromIntegral i))
    _ -> fail "Expected an integer token"

convToStr :: ParsecT [Token] State IO(Token)
convToStr = do 
  fun <- toStrToken
  l <- beginpToken
  n <- expression
  r <- endpToken
  return (StringL (pos n) (show (valueInt n)))

convAbs :: ParsecT [Token] State IO(Token)
convAbs = do 
  fun <- absToken
  l <- beginpToken
  n <- expression
  r <- endpToken
  return (IntL (pos n) (abs (valueInt n)))

--AUX 

valueInt :: Token -> Integer
valueInt (IntL p n) = n
valueInt _ = error "Not an integer token"

valueFloat :: Token -> Float
valueFloat (FloatL p n) = n
valueFloat _ = error "Not an float token"

pos :: Token -> (Int,Int)
pos (IntL p n) = p 

negValue :: Token -> Token
negValue (BoolL p b) = (BoolL p (not b))
negValue (IntL p n) = (IntL p (-n))
negValue (FloatL p n) = (FloatL p (-n))
negValue _ = error "is not a value"

eval :: Token -> Token -> Token -> Token
eval (IntL p x) (Plus _) (IntL r y) = IntL p (x + y)
eval (IntL p x) (Minus _) (IntL r y) =  IntL p (x - y)
eval (IntL p x) (Times _) (IntL r y) =  IntL p (x * y)
eval (IntL p x) (Divides _) (IntL r y) =  IntL p (x `div` y)
eval (IntL p x) (Pow _) (IntL r y) = if y >= 0 then IntL p (x ^ y) else error "Type mismatch: change to float"
eval (FloatL p x) (Plus _) (FloatL r y) = FloatL p (x + y)
eval (FloatL p x) (Minus _) (FloatL r y) =  FloatL p (x - y)
eval (FloatL p x) (Times _) (FloatL r y) =  FloatL p (x * y)
eval (FloatL p x) (Divides _) (FloatL r y) =  FloatL p (x / y)
eval (FloatL p x) (Pow _) (IntL r y) = if y >= 0 then FloatL p (x ^ y) else  FloatL p (1 / (x ^ (-y)))
--BOOL
eval (BoolL p x) (And _) (BoolL r y) = BoolL p (x &&  y)
eval (BoolL p x) (Or _) (BoolL r y) = BoolL p (x || y)
eval (BoolL p x) (Xor _) (BoolL r y) = BoolL p (((not x) &&  y) || (x && (not y)) )
-- REL
eval (BoolL p x) (Eq r) (BoolL q y) = (BoolL p (x == y))
eval (BoolL p x) (Neq r) (BoolL q y) = (BoolL p (not (x == y)))
eval (FloatL p x) (Leq r) (FloatL q y) = (BoolL p (x <= y))
eval (FloatL p x) (Geq r) (FloatL q y) = (BoolL p (x >= y))
eval (FloatL p x) (Less r) (FloatL q y) = (BoolL p (x < y))
eval (FloatL p x) (Greater r) (FloatL q y) = (BoolL p (x > y))
eval (FloatL p x) (Eq r) (FloatL q y) = (BoolL p (x == y))
eval (FloatL p x) (Neq r) (FloatL q y) = (BoolL p (not(x == y)))
eval (IntL p x) (Leq r) (IntL q y) = (BoolL p (x <= y))
eval (IntL p x) (Geq r) (IntL q y) = (BoolL p (x >= y))
eval (IntL p x) (Less r) (IntL q y) = (BoolL p (x < y))
eval (IntL p x) (Greater r) (IntL q y) = (BoolL p (x > y))
eval (IntL p x) (Eq r) (IntL q y) = (BoolL p (x == y))
eval (IntL p x) (Neq r) (IntL q y) = (BoolL p (x /= y)) 
eval (CharL p x) (Leq r) (CharL q y) = (BoolL p (x <= y))
eval (CharL p x) (Geq r) (CharL q y) = (BoolL p (x >= y))
eval (CharL p x) (Less r) (CharL q y) = (BoolL p (x < y))
eval (CharL p x) (Greater r) (CharL q y) = (BoolL p (x > y))
eval (CharL p x) (Eq r) (CharL q y) = (BoolL p (x == y))
eval (CharL p x) (Neq r) (CharL q y) = (BoolL p (not(x == y))) 
eval _ _ _ = error "Type mismatch"