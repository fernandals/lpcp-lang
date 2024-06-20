{-# LANGUAGE RecordWildCards #-}

module Utils where

import Data.List (intercalate)
import Lexer

typeof :: Token -> String
typeof (IntL {..}) = "int"
typeof (Int {..}) = "int"
typeof (FloatL {..}) = "float"
typeof (Float {..}) = "float"
typeof (E {..}) = "error"
typeof _ = ""

scopeNameBlock :: String -> String -> String
scopeNameBlock env_parent block_name = env_parent ++ "._" ++ block_name ++ "_"

scopeNameVar :: String -> String -> String
scopeNameVar static_parent id = static_parent ++ "." ++ id

split :: String -> [String]
split "" = [""]
split (c : cs)
  | c == '.' = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = split cs

destruct :: [String] -> String
destruct [] = ""
destruct [x, y] = ""
destruct (x : y : xs) = intercalate "." (reverse xs) ++ "." ++ x

parentScopeVar :: String -> String
parentScopeVar = destruct . reverse . split
