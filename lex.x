{
module Lex where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

-- Keywords
  "module"                               { \s -> Module}
  "mut"                                  { \s -> Mut }
  "let"                                  { \s -> Let }
  "if"                                   { \s -> If}
  "then"                                 { \s -> Then}
  "else"                                 { \s -> Else}
  "elif"                                 { \s -> Elif}
  "while"                                { \s -> While}
  "do"                                   { \s -> Do}
  "for"                                  { \s -> For}
  "loop"                                 { \s -> Loop}
  "return"                               { \s -> Return}
  "continue"                             { \s -> Continue}
   "break"                               { \s -> Break}


-- types
  "double"                              { \s -> Type s}
  "float"                               { \s -> Type s}
  "long"                                { \s -> Type s}
  "int"                                 { \s -> Type s}
  "char"                                { \s -> Type s}
  "string"                              { \s -> Type s}

--operators ari
 \+                                { \s -> Plus }
 \-                                { \s -> Minus }
 \*                                { \s -> Times }
 \/                                { \s -> Divide}
 \^                                { \s -> Pow}

--operators bool
 "or"                                { \s -> Or }
 "and"                                { \s -> And }
 "xor"                                { \s -> Xor }
 "not"                                { \s -> Not }


-- others
  $white+                              ;
  "--".*                               ;
  ":"                                  { \s -> Colon}
  ";"                                  { \s -> SemiColon}
  \=                                 { \s -> Assign}
  "{"                                  { \s -> Begin }
  "}"                                  { \s -> End }


-- ids and numbers
  $digit+                              { \s -> Int (read s) }
  $alpha [$alpha $digit \_ \']*        { \s -> Id s }
  \" $alpha [$alpha $digit ! \_ \']* \"  { \s -> String s}

{

data Token =
  Module  |
  Mut     |
  Let |
  If  |
  Then |
  Else |
  Elif |
  While |
  For |
  Do |
  Loop |
  Break |
  Continue |
  Return |
  Int Int |
  Plus    |
  Minus |
  Times |
  Divide |
  Pow |
  And |
  Or |
  Not |
  Xor |
  Colon   |
  SemiColon |
  Assign    | 
  Begin |
  End |
  Type String |
  Id String |
  String String
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}

-- alex lex.x -o Lex.hs
-- ghc -o teste teste.hs
-- ./teste program.txt