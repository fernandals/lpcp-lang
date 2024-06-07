{
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

    -- Keywords
  "module"                                { \p s -> Module $ getLC p }
  "mut"                                   { \p s -> Mut $ getLC p }
  "let"                                   { \p s -> Let $ getLC p }
  "if"                                    { \p s -> If $ getLC p }
  "then"                                  { \p s -> Then $ getLC p }
  "else"                                  { \p s -> Else $ getLC p }
  "elif"                                  { \p s -> Elif $ getLC p }
  "while"                                 { \p s -> While $ getLC p }
  "do"                                    { \p s -> Do $ getLC p }
  "for"                                   { \p s -> For $ getLC p }
  "loop"                                  { \p s -> Loop $ getLC p }
  "return"                                { \p s -> Return $ getLC p }
  "continue"                              { \p s -> Continue $ getLC p }
  "break"                                 { \p s -> Break $ getLC p }


  -- Types
  "double"                                { \p s -> Type $ getLC p }
  "float"                                 { \p s -> Float $ getLC p }
  "long"                                  { \p s -> Type $ getLC p }
  "int"                                   { \p s -> Int $ getLC p }
  "char"                                  { \p s -> Type $ getLC p }
  "string"                                { \p s -> String $ getLC p }
  "bool"                                  { \p s -> Bool $ getLC p }

  -- Arith operators
  \+                                      { \p s -> Plus $ getLC p }
  \-                                      { \p s -> Minus $ getLC p }
  \*                                      { \p s -> Times $ getLC p }
  \/                                      { \p s -> Divides $ getLC p }
  \^                                      { \p s -> Pow $ getLC p }
  "("                                     { \p s -> BeginP $ getLC p }
  ")"                                     { \p s -> EndP $ getLC p }

  -- Boolean operators
 "or"                                     { \p s -> Or $ getLC p }
 "and"                                    { \p s -> And $ getLC p }
 "xor"                                    { \p s -> Xor $ getLC p }
 "not"                                    { \p s -> Not $ getLC p }


  -- Others
  $white+                                 ;
  "--".*                                  ;
  ":"                                     { \p s -> Colon $ getLC p }
  ";"                                     { \p s -> SemiColon $ getLC p }
  \=                                      { \p s -> Assign $ getLC p }
  "{"                                     { \p s -> Begin $ getLC p }
  "}"                                     { \p s -> End $ getLC p }


  -- Ids and numbers
  $digit+                                 { \p s -> IntL (getLC p) (read s) }
  $digit+\.$digit                         { \p s -> FloatL (getLC p) (read s) }
  "true" | "false"                        { \p s -> BoolL (getLC p) (readbool s) }
  $alpha [$alpha $digit \_ \']*           { \p s -> Id (getLC p) s }
  \".*\"                                  { \p s -> StringL (getLC p) s }

{

type Pos = (Int, Int)

data Token
    = Module Pos
    -- Commands and flow
    | Mut Pos
    | Let Pos
    | If Pos
    | Then Pos
    | Else Pos
    | Elif Pos
    | While Pos
    | For Pos
    | Do Pos
    | Loop Pos
    | Break Pos
    | Continue Pos
    | Return Pos
    -- Types
    | Type Pos --placeholder
    | Int Pos
    | Float Pos
    | String Pos
    | Bool Pos
    -- Literals
    | IntL Pos Integer
    | FloatL Pos Float
    | StringL Pos String
    | BoolL Pos Bool
    -- Operators
    | Plus Pos
    | Minus Pos
    | Times Pos
    | Divides Pos
    | Pow Pos
    | Mod Pos
    | And Pos
    | Or Pos
    | Xor Pos
    | Not Pos
    -- Names and blocks and such
    | Colon Pos
    | SemiColon Pos
    | Assign Pos
    | Begin Pos
    | End Pos
    | BeginP Pos
    | EndP Pos
    | Id Pos String
    deriving ( Show )

instance Eq Token where
    (Id _ s) == (Id _ s') = s == s'

-- helpers
readbool :: String -> Bool
readbool "true" = True
readbool "false" = False

getLC :: AlexPosn -> Pos
getLC (AlexPn _ l c) = (l, c)

getTokens fn = unsafePerformIO $ getTokensAux fn
getTokensAux fn = do fh <- openFile fn ReadMode
                     s <- hGetContents fh
                     return $ alexScanTokens s
}
