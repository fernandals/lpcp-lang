module Builtin where

import Lexer
import Text.Parsec
import Tokens

toFloatFun :: ParsecT [Token] st IO Token
toFloatFun = tokenPrim show updatePos get_token
  where
    get_token (ToFloat pos) = Just $ ToFloat pos
    get_token _ = Nothing

toStrFun :: ParsecT [Token] st IO Token
toStrFun = tokenPrim show updatePos get_token
  where
    get_token (ToStr pos) = Just $ ToStr pos
    get_token _ = Nothing

absFun :: ParsecT [Token] st IO Token
absFun = tokenPrim show updatePos get_token
  where
    get_token (Abs pos) = Just $ Abs pos
    get_token _ = Nothing

printFun :: ParsecT [Token] st IO Token
printFun = tokenPrim show updatePos get_token
  where
    get_token (Print pos) = Just $ Print pos
    get_token _ = Nothing

printLnFun :: ParsecT [Token] st IO Token
printLnFun = tokenPrim show updatePos get_token
  where
    get_token (PrintLn pos) = Just $ PrintLn pos
    get_token _ = Nothing

printFFun :: ParsecT [Token] st IO Token
printFFun = tokenPrim show updatePos get_token
  where
    get_token (PrintF pos) = Just $ PrintF pos
    get_token _ = Nothing

getIntFun :: ParsecT [Token] st IO Token
getIntFun = tokenPrim show updatePos get_token
  where
    get_token (GetInt pos) = Just $ GetInt pos
    get_token _ = Nothing

getFloatFun :: ParsecT [Token] st IO Token
getFloatFun = tokenPrim show updatePos get_token
  where
    get_token (GetFloat pos) = Just $ GetFloat pos
    get_token _ = Nothing

getCharFun :: ParsecT [Token] st IO Token
getCharFun = tokenPrim show updatePos get_token
  where
    get_token (GetChar pos) = Just $ GetChar pos
    get_token _ = Nothing

getStringFun :: ParsecT [Token] st IO Token
getStringFun = tokenPrim show updatePos get_token
  where
    get_token (GetString pos) = Just $ GetString pos
    get_token _ = Nothing
