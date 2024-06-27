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
typeof' (R name _) = name

typeof :: Token -> String
typeof (LiteralValue p v) = typeof' v
typeof (Int {..}) = "int"
typeof (Float {..}) = "float"
typeof (Bool {..}) = "bool"
typeof (Char {..}) = "char"
typeof (String {..}) = "string"
typeof (RecordType {..}) = name typeRecord
typeof (E {..}) = "error"
typeof _ = ""

typeofToken :: Token -> String
typeofToken (LiteralValue _ v) = typeof' v
typeofToken (RecordType _ id) = name id
typeofToken _ = "unknown"

tokenToType :: Token -> Type
tokenToType (LiteralValue _ v) = v
tokenToType x = error $ "Expected a literal value" ++ (name x)

getRecordName :: Token -> String
getRecordName (Id _ n) = n
getRecordName (RecordType _ (Id _ n)) = n
getRecordName _ = error "Token does not have a name"

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
destruct [] = ""
destruct [x, y] = ""
destruct (x : y : xs) = intercalate "." (reverse xs) ++ "." ++ x

parentScopeVar :: String -> String
parentScopeVar = destruct . reverse . split
