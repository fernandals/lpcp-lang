-- Parsers for built-in function tokens

module Builtin where

import Lexer
import Tokens
import Text.Parsec

-- | Parses the main function token.
-- The name of the main function is actually a special token of the language.
mainFun :: ParsecT [Token] st IO Token
mainFun = tokenPrim show updatePos get_token
  where
    get_token (Main pos) = Just $ Main pos
    get_token _ = Nothing

-- Conversion function tokens
-----------------------------

-- | Parses the convertion to Float function token
toFloatFun :: ParsecT [Token] st IO Token
toFloatFun = tokenPrim show updatePos get_token
  where
    get_token (ToFloat pos) = Just $ ToFloat pos
    get_token _ = Nothing

-- | Parses the convertion to String function token
toStrFun :: ParsecT [Token] st IO Token
toStrFun = tokenPrim show updatePos get_token
  where
    get_token (ToStr pos) = Just $ ToStr pos
    get_token _ = Nothing

-- | Parses the absolute value function token
absFun :: ParsecT [Token] st IO Token
absFun = tokenPrim show updatePos get_token
  where
    get_token (Abs pos) = Just $ Abs pos
    get_token _ = Nothing

-- Output function tokens
-------------------------

-- | Parses the print function token
printFun :: ParsecT [Token] st IO Token
printFun = tokenPrim show updatePos get_token
  where
    get_token (Print pos) = Just $ Print pos
    get_token _ = Nothing

-- | Parses the printLn function token
printLnFun :: ParsecT [Token] st IO Token
printLnFun = tokenPrim show updatePos get_token
  where
    get_token (PrintLn pos) = Just $ PrintLn pos
    get_token _ = Nothing

-- | Parses the printF function token
printFFun :: ParsecT [Token] st IO Token
printFFun = tokenPrim show updatePos get_token
  where
    get_token (PrintF pos) = Just $ PrintF pos
    get_token _ = Nothing

-- Input function tokens
------------------------

-- | Parses the getInt function token
getIntFun :: ParsecT [Token] st IO Token
getIntFun = tokenPrim show updatePos get_token
  where
    get_token (GetInt pos) = Just $ GetInt pos
    get_token _ = Nothing

-- | Parses the getFloat function token
getFloatFun :: ParsecT [Token] st IO Token
getFloatFun = tokenPrim show updatePos get_token
  where
    get_token (GetFloat pos) = Just $ GetFloat pos
    get_token _ = Nothing

-- | Parses the getChar function token
getCharFun :: ParsecT [Token] st IO Token
getCharFun = tokenPrim show updatePos get_token
  where
    get_token (GetChar pos) = Just $ GetChar pos
    get_token _ = Nothing

-- | Parses the getString function token
getStringFun :: ParsecT [Token] st IO Token
getStringFun = tokenPrim show updatePos get_token
  where
    get_token (GetString pos) = Just $ GetString pos
    get_token _ = Nothing

-- List tokens
------------------------

lengthFun:: ParsecT [Token] st IO Token
lengthFun = tokenPrim show updatePos get_token
  where
    get_token (Length pos) = Just $ Length pos
    get_token _ = Nothing

appendFun:: ParsecT [Token] st IO Token
appendFun = tokenPrim show updatePos get_token
  where
    get_token (Append pos) = Just $ Append pos
    get_token _ = Nothing

prependFun:: ParsecT [Token] st IO Token
prependFun = tokenPrim show updatePos get_token
  where
    get_token (Prepend pos) = Just $ Prepend pos
    get_token _ = Nothing
