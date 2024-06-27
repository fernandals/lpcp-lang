-- Parsers for language tokens

module Tokens where

import Lexer
import System.IO.Unsafe
import Text.Parsec
import Text.Parsec (ParsecT)

-- Keyword tokens
---------------------------

-- | Parses the module token
-- The module token defines a new program
moduleToken :: ParsecT [Token] st IO Token
moduleToken = tokenPrim show updatePos get_token
  where
    get_token (Module pos) = Just $ Module pos
    get_token _ = Nothing

-- | Parses if token
ifToken :: ParsecT [Token] st IO Token
ifToken = tokenPrim show updatePos get_token
  where
    get_token (If pos) = Just $ If pos
    get_token _ = Nothing

-- | Parses then token
thenToken :: ParsecT [Token] st IO Token
thenToken = tokenPrim show updatePos get_token
  where
    get_token (Then pos) = Just $ Then pos
    get_token _ = Nothing

-- | Parses elif token
elifToken :: ParsecT [Token] st IO Token
elifToken = tokenPrim show updatePos get_token
  where
    get_token (Elif pos) = Just $ Elif pos
    get_token _ = Nothing

-- | Parses else token
elseToken :: ParsecT [Token] st IO Token
elseToken = tokenPrim show updatePos get_token
  where
    get_token (Else pos) = Just $ Else pos
    get_token _ = Nothing

-- | Parses while token
whileToken :: ParsecT [Token] st IO Token
whileToken = tokenPrim show updatePos get_token
  where
    get_token (While pos) = Just $ While pos
    get_token _ = Nothing

-- | Parses do token
doToken :: ParsecT [Token] st IO Token
doToken = tokenPrim show updatePos get_token
  where
    get_token (Do pos) = Just $ Do pos
    get_token _ = Nothing

-- Type tokens
--------------

-- | Parses int token
-- literally
intToken :: ParsecT [Token] st IO Token
intToken = tokenPrim show updatePos get_token
  where
    get_token (Int pos) = Just $ Int pos
    get_token _ = Nothing

-- | Parses float token
-- literally
floatToken :: ParsecT [Token] st IO Token
floatToken = tokenPrim show updatePos get_token
  where
    get_token (Float pos) = Just $ Float pos
    get_token _ = Nothing

-- | Parses bool token
-- literally
boolToken :: ParsecT [Token] st IO Token
boolToken = tokenPrim show updatePos get_token
  where
    get_token (Bool pos) = Just $ Bool pos
    get_token _ = Nothing

-- | Parses char token
-- literally
charToken :: ParsecT [Token] st IO Token
charToken = tokenPrim show updatePos get_token
  where
    get_token (Char pos) = Just $ Char pos
    get_token _ = Nothing

-- | Parses string token
-- literally
stringToken :: ParsecT [Token] st IO Token
stringToken = tokenPrim show updatePos get_token
  where
    get_token (String pos) = Just $ String pos
    get_token _ = Nothing

-- Arithmetic tokens
--------------------

-- | Parses plus (+) token
plusToken :: ParsecT [Token] st IO Token
plusToken = tokenPrim show updatePos get_token
  where
    get_token (Plus pos) = Just $ Plus pos
    get_token _ = Nothing

-- | Parses minus (-) token
minusToken :: ParsecT [Token] st IO Token
minusToken = tokenPrim show updatePos get_token
  where
    get_token (Minus pos) = Just $ Minus pos
    get_token _ = Nothing

-- | Parses times (*) token
timesToken :: ParsecT [Token] st IO Token
timesToken = tokenPrim show updatePos get_token
  where
    get_token (Times pos) = Just $ Times pos
    get_token _ = Nothing

-- | Parses divides (/) token
dividesToken :: ParsecT [Token] st IO Token
dividesToken = tokenPrim show updatePos get_token
  where
    get_token (Divides pos) = Just $ Divides pos
    get_token _ = Nothing

-- | Parses pow (^) token
powToken :: ParsecT [Token] st IO Token
powToken = tokenPrim show updatePos get_token
  where
    get_token (Pow pos) = Just $ Pow pos
    get_token _ = Nothing

-- | Parses modulos (%) token
modulosToken :: ParsecT [Token] st IO Token
modulosToken = tokenPrim show updatePos get_token
  where
    get_token (Modulos pos) = Just $ Modulos pos
    get_token _ = Nothing

-- Bracket tokens
-----------------

-- | Parses begin parenthesis (() token
beginpToken :: ParsecT [Token] st IO Token
beginpToken = tokenPrim show updatePos get_token
  where
    get_token (BeginP pos) = Just $ BeginP pos
    get_token _ = Nothing

-- | Parses end parenthesis ()) token
endpToken :: ParsecT [Token] st IO Token
endpToken = tokenPrim show updatePos get_token
  where
    get_token (EndP pos) = Just $ EndP pos
    get_token _ = Nothing

-- | Parses begin bracket ({) token
beginBToken :: ParsecT [Token] st IO Token
beginBToken = tokenPrim show updatePos get_token
  where
    get_token (BeginB pos) = Just $ BeginB pos
    get_token _ = Nothing

-- | Parses end bracket (}) token
endBToken :: ParsecT [Token] st IO Token
endBToken = tokenPrim show updatePos get_token
  where
    get_token (EndB pos) = Just $ EndB pos
    get_token _ = Nothing

-- Boolean tokens
-----------------

-- | Parses and token
andToken :: ParsecT [Token] st IO Token
andToken = tokenPrim show updatePos get_token
  where
    get_token (And pos) = Just $ And pos
    get_token _ = Nothing

-- | Parses or token
orToken :: ParsecT [Token] st IO Token
orToken = tokenPrim show updatePos get_token
  where
    get_token (Or pos) = Just $ Or pos
    get_token _ = Nothing

-- | Parses not token
notToken :: ParsecT [Token] st IO Token
notToken = tokenPrim show updatePos get_token
  where
    get_token (Not pos) = Just $ Not pos
    get_token _ = Nothing

-- | Parses xor token
xorToken :: ParsecT [Token] st IO Token
xorToken = tokenPrim show updatePos get_token
  where
    get_token (Xor pos) = Just $ Xor pos
    get_token _ = Nothing

-- Relation tokens
------------------

-- | Parses greater or equal (>=) token
geqToken :: ParsecT [Token] st IO Token
geqToken = tokenPrim show updatePos get_token
  where
    get_token (Geq pos) = Just $ Geq pos
    get_token _ = Nothing

-- | Parses less or equal (<=) token
leqToken :: ParsecT [Token] st IO Token
leqToken = tokenPrim show updatePos get_token
  where
    get_token (Leq pos) = Just $ Leq pos
    get_token _ = Nothing

-- | Parses greater (>) token
greaterToken :: ParsecT [Token] st IO Token
greaterToken = tokenPrim show updatePos get_token
  where
    get_token (Greater pos) = Just $ Greater pos
    get_token _ = Nothing

-- | Parses less (<) token
lessToken :: ParsecT [Token] st IO Token
lessToken = tokenPrim show updatePos get_token
  where
    get_token (Less pos) = Just $ Less pos
    get_token _ = Nothing

-- | Parses equal (==) token
eqToken :: ParsecT [Token] st IO Token
eqToken = tokenPrim show updatePos get_token
  where
    get_token (Eq pos) = Just $ Eq pos
    get_token _ = Nothing

-- | Parses not equal (!=) token
neqToken :: ParsecT [Token] st IO Token
neqToken = tokenPrim show updatePos get_token
  where
    get_token (Neq pos) = Just $ Neq pos
    get_token _ = Nothing

-- Symbolic tokens
------------------

-- | Parses comma (,) token
-- The comma token is just a comma
commaToken :: ParsecT [Token] st IO Token
commaToken = tokenPrim show updatePos get_token
  where
    get_token (Comma pos) = Just $ Comma pos
    get_token _ = Nothing

-- | Parses assign (=) token
assignToken :: ParsecT [Token] st IO Token
assignToken = tokenPrim show updatePos get_token
  where
    get_token (Assign pos) = Just $ Assign pos
    get_token _ = Nothing

-- | Parses colon (:) token
-- The colon token indicates the type of an id
colonToken :: ParsecT [Token] st IO Token
colonToken = tokenPrim show updatePos get_token
  where
    get_token (Colon pos) = Just $ Colon pos
    get_token _ = Nothing

-- | Parses arrow (->) token
-- The arrow indicates the return type of a function
arrowToken :: ParsecT [Token] st IO Token
arrowToken = tokenPrim show updatePos get_token
  where
    get_token (Arrow pos) = Just $ Arrow pos
    get_token _ = Nothing

-- | Parses ampersand (&) token
-- The ampersand indicates a reference
amperToken :: ParsecT [Token] st IO Token
amperToken = tokenPrim show updatePos get_token
  where
    get_token (Amper pos) = Just $ Amper pos
    get_token _ = Nothing


-- Variable-related tokens 
--------------------------

-- | Parses identifier tokens
-- This parser has an id name associated with
idToken :: ParsecT [Token] st IO Token
idToken = tokenPrim show updatePos get_token
  where
    get_token (Id pos name) = Just $ Id pos name
    get_token _ = Nothing

-- | Parses let token
-- The let modifier is used to declare immutable variables
letToken :: ParsecT [Token] st IO Token
letToken = tokenPrim show updatePos get_token
  where
    get_token (Let pos) = Just $ Let pos
    get_token _ = Nothing

-- | Parses mut token
-- The mut modifier is used to declare mutable variables
mutToken :: ParsecT [Token] st IO Token
mutToken = tokenPrim show updatePos get_token
  where
    get_token (Mut pos) = Just $ Mut pos
    get_token _ = Nothing

-- Function-related tokens
--------------------------

-- | Parses fun token
-- The fun token is used to declare functions
funToken :: ParsecT [Token] st IO Token
funToken = tokenPrim show updatePos get_token
  where
    get_token (Fun pos) = Just $ Fun pos
    get_token _ = Nothing

-- | Parses return token
-- The return token indicates the value returned by a function
returnToken :: ParsecT [Token] st IO Token
returnToken = tokenPrim show updatePos get_token
  where
    get_token (Return pos) = Just $ Return pos
    get_token _ = Nothing

-- Literal tokens
-----------------

-- | Parses literals
-- Literal values are numbers, boolean values, characters and strings
literalValueToken :: ParsecT [Token] st IO Token
literalValueToken = tokenPrim show updatePos get_token
  where
    get_token (LiteralValue pos val) = Just $ LiteralValue pos val
    get_token _ = Nothing


-- | Updates position of tokens
updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos _ (tok : _) = pos
updatePos pos _ [] = pos
