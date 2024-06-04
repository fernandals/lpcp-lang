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

intLToken :: ParsecT [Token] st IO Token
intLToken = tokenPrim show updatePos get_token
  where
    get_token (IntL pos n) = Just $ IntL pos n
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

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos _ (tok : _) = pos -- necessita melhoria
updatePos pos _ [] = pos
