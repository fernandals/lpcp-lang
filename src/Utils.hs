{-# LANGUAGE RecordWildCards #-}

module Utils where

import Data.List (intercalate)
import Lexer

typeErrorMsg :: Pos -> Token -> Token -> String
typeErrorMsg pos expected_type actual_type =
  "Type mismatch at "
    ++ show pos
    ++ ".\nExpected "
    ++ typeof expected_type
    ++ ", got "
    ++ typeof actual_type
    ++ ".\n"

typeErrorExprMsg :: Pos -> String -> Type -> Type -> String
typeErrorExprMsg pos op_name t t' =
  "Type mismatch at evaluation of expression in "
    ++ show pos
    ++ ".\nCant't apply "
    ++ op_name
    ++ " to "
    ++ typeof' t
    ++ " and "
    ++ typeof' t'
    ++ ".\n"

typeErrorRelation :: Pos -> String -> Type -> Type -> String
typeErrorRelation pos rel_name t t' =
  "Type mismatch at evaluation of expression in "
    ++ show pos
    ++ ".\nCant't compare "
    ++ typeof' t
    ++ " and "
    ++ typeof' t'
    ++ " with "
    ++ rel_name
    ++ ".\n"

typeErrorUnary :: Pos -> String -> Type -> String
typeErrorUnary pos op_name t =
  "Type mismatch at evaluation of expression in "
    ++ show pos
    ++ ".\nCan't apply "
    ++ op_name
    ++ " to "
    ++ typeof' t
    ++ ".\n"

unexpectedOperatorError :: Pos -> Token -> String
unexpectedOperatorError pos t =
  "Unexpeted "
    ++ show t
    ++ "on position "
    ++ show pos
    ++ ".\n"

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
