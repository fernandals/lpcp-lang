-- Useful functions

{-# LANGUAGE RecordWildCards #-}

module Utils where

import Lexer
import Data.List (intercalate)

-- | Checks whether the interpreter should execute the code.
-- Only when the flag is active or inside the global scope.
canExecute :: Bool -> String -> Bool
canExecute flag act_name = flag || ("_global_" == act_name)

-- | Returns whether a given Boolean token has a true value within it.
isTrue :: Token -> Bool
isTrue (LiteralValue p (B b)) = b

-- | Checks the type of tokens provided.
-- New type version.
typeof' :: Type -> String
typeof' (I _) = "int"
typeof' (F _) = "float"
typeof' (B _) = "bool"
typeof' (C _) = "char"
typeof' (S _) = "string"

-- | Checks the type of tokens provided.
-- Old type version.
typeof :: Token -> String
typeof (LiteralValue p v) = typeof' v
typeof (Int {..}) = "int"
typeof (Float {..}) = "float"
typeof (Bool {..}) = "bool"
typeof (Char {..}) = "char"
typeof (String {..}) = "string"
typeof (E {..}) = "error"
typeof (Reference {..}) = "&" ++ typeof rtype
typeof _ = ""

-- | Extracts the token inside a Maybe type.
extractToken :: Maybe Token -> Token
extractToken (Just t) = t
extractToken Nothing = E (0, 0)

-- | Sets the scope name for a block.
-- e.g. _globa_._main_._if_
scopeNameBlock :: String -> String -> String
scopeNameBlock env_parent block_name = env_parent ++ "._" ++ block_name ++ "_"

-- | Sets the scope name for a variable.
-- e.g. _globa_._main_._f_.x
scopeNameVar :: String -> String -> String
scopeNameVar static_parent id = static_parent ++ "." ++ id

-- | Checks whether the input is within a given scope.
inScope :: String -> String -> Bool
inScope scope_name name = scope_name == scope name
  where
    scope = intercalate "." . init . split

-- | Helper function to remove dots from scope name.
split :: String -> [String]
split "" = [""]
split (c : cs)
  | c == '.' = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = split cs

-- | Helper function to remove a level from the scope name while keeping the variable name in it.
destruct :: [String] -> String
destruct [x, y] = ""
destruct (x : y : xs) = intercalate "." (reverse xs) ++ "." ++ x
destruct _ = ""

-- | Provides the scope name of a variable's static parent.
parentScopeVar :: String -> String
parentScopeVar = destruct . reverse . split

-- | Provides the scope name of a blocks's static parent
parentScopeBlock :: String -> String
parentScopeBlock = intercalate "." . init . split

-- | Helper function to define function names.
-- Since functions can only be declared within the global scope.
functionName :: String -> String
functionName f_name = "_global_._" ++ f_name ++ "_"

-- | Helper function to remove block names from a scope to find the associated function.
removeBlockNames :: String -> String
removeBlockNames = intercalate "." . reverse . removeBlock . reverse . split
  where 
    removeBlock (x:xs)
      | x == "_if_" = removeBlock xs
      | x == "_elif_" = removeBlock xs
      | x == "_else_" = removeBlock xs
      | x == "_while_" = removeBlock xs
      | otherwise = x:xs