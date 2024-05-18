module Parser where

import Lexer
import Text.Parsec
import Tokens

globals :: Parsec [Token] st [Token]
globals = do
  modifier <- try letToken <|> mutToken
  name <- idToken
  colon <- colonToken
  decltype <- intToken
  assign <- assignToken
  expression <- intLToken
  return
    [ modifier,
      name,
      colon,
      decltype,
      assign,
      expression
    ]

program :: Parsec [Token] st [Token]
program = do
  p <- moduleToken
  pn <- idToken
  a <- globals
  eof
  return ([p, pn] ++ a)

parser :: [Token] -> Either ParseError [Token]
parser = runParser program () "Error"
