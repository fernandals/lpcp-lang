module Tokens where

import Lexer
import System.IO.Unsafe
import Text.Parsec

moduleToken :: Parsec [Token] st Token
moduleToken = tokenPrim show update_pos get_token
  where
    get_token (Module pos) = Just $ Module pos
    get_token _ = Nothing

idToken :: Parsec [Token] st Token
idToken = tokenPrim show update_pos get_token
  where
    get_token (Id pos name) = Just $ Id pos name
    get_token _ = Nothing

mutToken :: Parsec [Token] st Token
mutToken = tokenPrim show update_pos get_token
  where
    get_token (Mut pos) = Just $ Mut pos
    get_token _ = Nothing

assignToken :: Parsec [Token] st Token
assignToken = tokenPrim show update_pos get_token
  where
    get_token (Assign pos) = Just $ Assign pos
    get_token _ = Nothing

colonToken :: Parsec [Token] st Token
colonToken = tokenPrim show update_pos get_token
  where
    get_token (Colon pos) = Just $ Colon pos
    get_token _ = Nothing

intToken :: Parsec [Token] st Token
intToken = tokenPrim show update_pos get_token
  where
    get_token (Int pos) = Just $ Int pos
    get_token _ = Nothing

intLToken :: Parsec [Token] st Token
intLToken = tokenPrim show update_pos get_token
  where
    get_token (IntL pos n) = Just $ IntL pos n
    get_token _ = Nothing

letToken :: Parsec [Token] st Token
letToken = tokenPrim show update_pos get_token
  where
    get_token (Let pos) = Just $ Let pos
    get_token _ = Nothing

ifToken :: Parsec [Token] st Token
ifToken = tokenPrim show update_pos get_token
  where
    get_token (If pos) = Just $ If pos
    get_token _ = Nothing

thenToken :: Parsec [Token] st Token
thenToken = tokenPrim show update_pos get_token
  where
    get_token (Then pos) = Just $ Then pos
    get_token _ = Nothing

elseToken :: Parsec [Token] st Token
elseToken = tokenPrim show update_pos get_token
  where
    get_token (Else pos) = Just $ Else pos
    get_token _ = Nothing

elifToken :: Parsec [Token] st Token
elifToken = tokenPrim show update_pos get_token
  where
    get_token (Elif pos) = Just $ Elif pos
    get_token _ = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok : _) = pos -- necessita melhoria
update_pos pos _ [] = pos
