module Types where

import Lexer
import State
import Text.Parsec hiding (State)
import Tokens

-- Types involved in declarations
types :: ParsecT [Token] State IO Token
types = intToken <|> floatToken <|> boolToken <|> charToken <|> stringToken
