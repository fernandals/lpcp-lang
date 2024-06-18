{
{-# LANGUAGE RecordWildCards #-}

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
  "int"                                   { \p s -> Int $ getLC p }
  "char"                                  { \p s -> Char $ getLC p }
  "string"                                { \p s -> String $ getLC p }
  "bool"                                  { \p s -> Bool $ getLC p }

  -- Arith operators
  \+                                      { \p s -> Plus $ getLC p }
  \-                                      { \p s -> Minus $ getLC p }
  \*                                      { \p s -> Times $ getLC p }
  \/                                      { \p s -> Divides $ getLC p }
  \^                                      { \p s -> Pow $ getLC p }
  \%                                      { \p s -> Modulos $ getLC p }
  "("                                     { \p s -> BeginP $ getLC p }
  ")"                                     { \p s -> EndP $ getLC p }

  -- Boolean operators
 "or"                                     { \p s -> Or $ getLC p }
 "and"                                    { \p s -> And $ getLC p }
 "xor"                                    { \p s -> Xor $ getLC p }
 "not"                                    { \p s -> Not $ getLC p }

 -- int functions 
 "toFloat"                                { \p s -> ToFloat $ getLC p}
 "toStr"                                  { \p s -> ToStr $ getLC p}
 "abs"                                    { \p s -> Abs $ getLC p}

 -- int relations
  ">="                                     { \p s -> Geq $ getLC p }
  "<="                                     { \p s -> Leq $ getLC p }
  ">"                                     { \p s -> Greater $ getLC p }
  "<"                                     { \p s -> Less $ getLC p }
  "=="                                     { \p s -> Eq $ getLC p }
  "!="                                     { \p s -> Neq $ getLC p }

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
  \'[$alpha $digit]\'                     { \p s -> CharL (getLC p) (s !! 1)}
  \".*\"                                  { \p s -> StringL (getLC p) (getStr s)}

{

type Pos = (Int, Int)

data Token
    = Module {pos :: Pos}
    -- Commands and flow
    | Mut {pos :: Pos}
    | Let {pos :: Pos}
    | If {pos :: Pos}
    | Then {pos :: Pos}
    | Else {pos :: Pos}
    | Elif {pos :: Pos}
    | While {pos :: Pos}
    | For {pos :: Pos}
    | Do {pos :: Pos}
    | Loop {pos :: Pos}
    | Break {pos :: Pos}
    | Continue {pos :: Pos}
    | Return {pos :: Pos}
    -- Types
    | Type {pos :: Pos} --placeholder
    | Int {pos :: Pos}
    | Float {pos :: Pos}
    | String {pos :: Pos}
    | Bool {pos :: Pos}
    | Char {pos :: Pos}
    -- Literals
    | IntL {pos :: Pos, int :: Integer}
    | FloatL {pos :: Pos, float :: Float}
    | StringL {pos :: Pos, string :: String}
    | BoolL {pos :: Pos, bool :: Bool}
    | CharL {pos :: Pos, char :: Char}
    -- Operators
    | Plus {pos :: Pos}
    | Minus {pos :: Pos}
    | Times {pos :: Pos}
    | Divides {pos :: Pos}
    | Modulos {pos :: Pos}
    | Pow {pos :: Pos}
    | Mod {pos :: Pos}
    | And {pos :: Pos}
    | Or {pos :: Pos}
    | Xor {pos :: Pos}
    | Not {pos :: Pos}
    -- Funcions
    | ToFloat {pos :: Pos}
    | ToStr {pos :: Pos}
    | Abs {pos :: Pos}
    -- Relations
    | Geq {pos :: Pos}
    | Leq {pos :: Pos}
    | Greater {pos :: Pos}
    | Less {pos :: Pos}
    | Eq {pos :: Pos}
    | Neq {pos :: Pos}
    -- Names and blocks and such
    | Colon {pos :: Pos}
    | SemiColon {pos :: Pos}
    | Assign {pos :: Pos}
    | Begin {pos :: Pos}
    | End {pos :: Pos}
    | BeginP {pos :: Pos}
    | EndP {pos :: Pos}
    | Id {pos :: Pos, name :: String}
    -- Error handling
    | E {pos :: Pos}

instance Eq Token where
    (Id {name = s}) == (Id {name = s'}) = s == s'

instance Show Token where
    show (IntL {..}) = show int
    show (FloatL {..}) = show float
    show (BoolL {..}) = show bool
    show (CharL {..}) = show char
    show (StringL {..}) = string
    show _ = ""

-- helpers
readbool :: String -> Bool
readbool "true" = True
readbool "false" = False

getLC :: AlexPosn -> Pos
getLC (AlexPn _ l c) = (l, c)

getStr :: String -> String
getStr str
    | length str > 1 = init (tail str)
    | otherwise = ""

getTokens fn = unsafePerformIO $ getTokensAux fn
getTokensAux fn = do fh <- openFile fn ReadMode
                     s <- hGetContents fh
                     return $ alexScanTokens s
}
