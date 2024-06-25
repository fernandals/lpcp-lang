{-# LANGUAGE RecordWildCards #-}

module Utils where

import Data.List (intercalate)
import Lexer

canExecute :: Bool -> String -> Bool
canExecute flag act_name = flag || ("_global_" == act_name)

isTrue :: Token -> Bool
isTrue (LiteralValue p (B b)) = b

typeof' :: Type -> String
typeof' (I _) = "int"
typeof' (F _) = "float"
typeof' (B _) = "bool"
typeof' (C _) = "char"
typeof' (S _) = "string"

typeof :: Token -> String
typeof (LiteralValue p v) = typeof' v
typeof (Int {..}) = "int"
typeof (Float {..}) = "float"
typeof (Bool {..}) = "bool"
typeof (Char {..}) = "char"
typeof (String {..}) = "string"
typeof (E {..}) = "error"
typeof _ = ""

extractToken :: Maybe Token -> Token
extractToken (Just t) = t
extractToken Nothing = E (0, 0)

scopeNameBlock :: String -> String -> String
scopeNameBlock env_parent block_name = env_parent ++ "._" ++ block_name ++ "_"

scopeNameVar :: String -> String -> String
scopeNameVar static_parent id = static_parent ++ "." ++ id

inScope :: String -> String -> Bool
inScope scope_name name = scope_name == scope name
  where
    scope = intercalate "." . init . split

split :: String -> [String]
split "" = [""]
split (c : cs)
  | c == '.' = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = split cs

destruct :: [String] -> String
destruct [x, y] = ""
destruct (x : y : xs) = intercalate "." (reverse xs) ++ "." ++ x
destruct _ = ""

parentScopeVar :: String -> String
parentScopeVar = destruct . reverse . split
