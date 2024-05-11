{
module Lex where
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

    -- Keywords
  "module"                                { \p s -> Module p }
  "mut"                                   { \p s -> Mut p }
  "let"                                   { \p s -> Let p }
  "if"                                    { \p s -> If p }
  "then"                                  { \p s -> Then p }
  "else"                                  { \p s -> Else p }
  "elif"                                  { \p s -> Elif p }
  "while"                                 { \p s -> While p }
  "do"                                    { \p s -> Do p }
  "for"                                   { \p s -> For p }
  "loop"                                  { \p s -> Loop p }
  "return"                                { \p s -> Return p }
  "continue"                              { \p s -> Continue p }
  "break"                                 { \p s -> Break p }


  -- Types
  "double"                                { \p s -> Type p s }
  "float"                                 { \p s -> Float p }
  "long"                                  { \p s -> Type p s }
  "int"                                   { \p s -> Int p }
  "char"                                  { \p s -> Type p s }
  "string"                                { \p s -> String p }
  "bool"                                  { \p s -> Bool p }

  -- Arith operators
  \+                                      { \p s -> Plus p }
  \-                                      { \p s -> Minus p }
  \*                                      { \p s -> Times p }
  \/                                      { \p s -> Divides p }
  \^                                      { \p s -> Pow p }

  -- Boolean operators
 "or"                                     { \p s -> Or p }
 "and"                                    { \p s -> And p }
 "xor"                                    { \p s -> Xor p }
 "not"                                    { \p s -> Not p }


  -- Others
  $white+                                 ;
  "--".*                                  ;
  ":"                                     { \p s -> Colon p }
  ";"                                     { \p s -> SemiColon p }
  \=                                      { \p s -> Assign p }
  "{"                                     { \p s -> Begin p }
  "}"                                     { \p s -> End p }


  -- Ids and numbers
  $digit+                                 { \p s -> IntL p (read s) }
  $digit+\.$digit                         { \p s -> FloatL p (read s) }
  "true" | "false"                        { \p s -> BoolL p (readbool s) }
  $alpha [$alpha $digit \_ \']*           { \p s -> Id p s }
  \".*\"                                  { \p s -> StringL p s }

{

-- helpers
readbool :: String -> Bool
readbool "true" = True
readbool "false" = False

data Token
    = Module AlexPosn
    -- Commands and flow
    | Mut AlexPosn
    | Let AlexPosn
    | If AlexPosn
    | Then AlexPosn
    | Else AlexPosn
    | Elif AlexPosn
    | While AlexPosn
    | For AlexPosn
    | Do AlexPosn
    | Loop AlexPosn
    | Break AlexPosn
    | Continue AlexPosn
    | Return AlexPosn
    -- Types
    | Type AlexPosn String --placeholder
    | Int AlexPosn
    | Float AlexPosn
    | String AlexPosn
    | Bool AlexPosn
    -- Literals
    | IntL AlexPosn Integer
    | FloatL AlexPosn Float
    | StringL AlexPosn String
    | BoolL AlexPosn Bool
    -- Operators
    | Plus AlexPosn
    | Minus AlexPosn
    | Times AlexPosn
    | Divides AlexPosn
    | Pow AlexPosn
    | Mod AlexPosn
    | And AlexPosn
    | Or AlexPosn
    | Xor AlexPosn
    | Not AlexPosn
    -- Names and blocks and such
    | Colon AlexPosn
    | SemiColon AlexPosn
    | Assign AlexPosn
    | Begin AlexPosn
    | End AlexPosn
    | Id AlexPosn String
    deriving ( Eq,Show )

main = do
  s <- getContents
  print (alexScanTokens s)
  
}

-- alex lex.x -o Lex.hs
-- ghc -o teste teste.hs
-- ./teste program.txt
