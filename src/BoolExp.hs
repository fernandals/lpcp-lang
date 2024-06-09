module BoolExp where

import Control.Monad.IO.Class (liftIO)
import Data.IntMap (update)
import Lexer
import State
import Text.Parsec hiding (State)
import Tokens

-- precisa corrigir pq as precedencias estao bugadas : true or (false and false) -> false

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
    n1 <- try bracketBoolExpr <|> unaBoolExpr <|> atomBoolExpr
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

leq :: Token -> Token -> Token
leq (IntL p x) (IntL q y) = (BoolL p (x <= y))

geq :: Token -> Token -> Token
geq (IntL p x) (IntL q y) = (BoolL p (x >= y))