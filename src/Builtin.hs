module Builtin where

import Lexer
import Text.Parsec
import Tokens

printFun :: ParsecT [Token] st IO Token
printFun = tokenPrim show updatePos get_token
  where
    get_token (Id pos name) =
      if name == "print"
        then Just $ Id pos name
        else Nothing
    get_token _ = Nothing

printlnFun :: ParsecT [Token] st IO Token
printlnFun = tokenPrim show updatePos get_token
  where
    get_token (Id pos name) =
      if name == "println"
        then Just $ Id pos name
        else Nothing
    get_token _ = Nothing
