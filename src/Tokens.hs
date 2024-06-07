module Tokens where

import Lexer
import System.IO.Unsafe
import Text.Parsec

moduleToken :: ParsecT [Token] st IO Token
moduleToken = tokenPrim show updatePos get_token
  where
    get_token (Module pos) = Just $ Module pos
    get_token _ = Nothing

idToken :: ParsecT [Token] st IO Token
idToken = tokenPrim show updatePos get_token
  where
    get_token (Id pos name) = Just $ Id pos name
    get_token _ = Nothing

mutToken :: ParsecT [Token] st IO Token
mutToken = tokenPrim show updatePos get_token
  where
    get_token (Mut pos) = Just $ Mut pos
    get_token _ = Nothing

assignToken :: ParsecT [Token] st IO Token
assignToken = tokenPrim show updatePos get_token
  where
    get_token (Assign pos) = Just $ Assign pos
    get_token _ = Nothing

colonToken :: ParsecT [Token] st IO Token
colonToken = tokenPrim show updatePos get_token
  where
    get_token (Colon pos) = Just $ Colon pos
    get_token _ = Nothing

intToken :: ParsecT [Token] st IO Token
intToken = tokenPrim show updatePos get_token
  where
    get_token (Int pos) = Just $ Int pos
    get_token _ = Nothing
  
floatToken :: ParsecT [Token] st IO Token
floatToken = tokenPrim show updatePos get_token
  where
    get_token (Float pos) = Just $ Float pos
    get_token _ = Nothing

intLToken :: ParsecT [Token] st IO Token
intLToken = tokenPrim show updatePos get_token
  where
    get_token (IntL pos n) = Just $ IntL pos n
    get_token _ = Nothing

floatLToken :: ParsecT [Token] st IO Token
floatLToken = tokenPrim show updatePos get_token
  where
    get_token (FloatL pos n) = Just $ FloatL pos n
    get_token _ = Nothing

letToken :: ParsecT [Token] st IO Token
letToken = tokenPrim show updatePos get_token
  where
    get_token (Let pos) = Just $ Let pos
    get_token _ = Nothing

ifToken :: ParsecT [Token] st IO Token
ifToken = tokenPrim show updatePos get_token
  where
    get_token (If pos) = Just $ If pos
    get_token _ = Nothing

thenToken :: ParsecT [Token] st IO Token
thenToken = tokenPrim show updatePos get_token
  where
    get_token (Then pos) = Just $ Then pos
    get_token _ = Nothing

elseToken :: ParsecT [Token] st IO Token
elseToken = tokenPrim show updatePos get_token
  where
    get_token (Else pos) = Just $ Else pos
    get_token _ = Nothing

elifToken :: ParsecT [Token] st IO Token
elifToken = tokenPrim show updatePos get_token
  where
    get_token (Elif pos) = Just $ Elif pos
    get_token _ = Nothing

-- Tokens para exp arimimeticas
plusToken :: ParsecT [Token] st IO Token
plusToken = tokenPrim show updatePos get_token
  where
    get_token (Plus pos) = Just $ Plus pos
    get_token _ = Nothing

minusToken :: ParsecT [Token] st IO Token
minusToken = tokenPrim show updatePos get_token
  where
    get_token (Minus pos) = Just $ Minus pos
    get_token _ = Nothing

timesToken :: ParsecT [Token] st IO Token
timesToken = tokenPrim show updatePos get_token
  where
    get_token (Times pos) = Just $ Times pos
    get_token _ = Nothing

dividesToken :: ParsecT [Token] st IO Token
dividesToken = tokenPrim show updatePos get_token
  where
    get_token (Divides pos) = Just $ Divides pos
    get_token _ = Nothing

powToken :: ParsecT [Token] st IO Token
powToken = tokenPrim show updatePos get_token
  where
    get_token (Pow pos) = Just $ Pow pos
    get_token _ = Nothing

beginpToken :: ParsecT [Token] st IO Token
beginpToken = tokenPrim show updatePos get_token
  where
    get_token (BeginP pos) = Just $ BeginP pos
    get_token _ = Nothing

endpToken :: ParsecT [Token] st IO Token
endpToken = tokenPrim show updatePos get_token
  where
    get_token (EndP pos) = Just $ EndP pos
    get_token _ = Nothing

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos _ (tok : _) = pos -- necessita melhoria
updatePos pos _ [] = pos
