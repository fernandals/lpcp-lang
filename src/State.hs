{-# LANGUAGE UnicodeSyntax #-}

module State where

import Lexer
import Utils

-- Wether it should execute or just parse
type ExecutionFlag = Bool

-- An entry to a variable in the table contains its modifier, type and value.
-- The symbol table contains a list of its versions, so it can support recursion.
-- The top of the stack of activations contains the most recent version of it, its executing version.
type SymbolEntry = (Token, Token, Token)

type Symbol = (String, [SymbolEntry])

type SymTable = [Symbol]

-- And activation stack entry just contains the name of the function that is executing in the given moment,
-- plus the depth of its activation, i.e. if its a recursive function, it'll call itself many times and this
-- will increase its depth.
-- The activation stack just stacks up subprograms activations.
type ActEntry = (String, Int)

type ActStack = [ActEntry]

-- An entry to the scope table contains the id of the entry and the id of its static parent.
-- To access things by scope, we just need to follow the static chain from our current id to all its static parents.
-- The global scope is an ancestor of everyone
type ScopeEntry = (String, String)

type ScopeTable = [ScopeEntry]

type TypeEntry = String

type TypeTable = [TypeEntry]

type SubpEntry = String

type SubpTable = [SubpEntry]

type State = (ExecutionFlag, SymTable, ActStack, ScopeTable, TypeTable, SubpTable)

getSymTable :: State -> SymTable
getSymTable (_, symt, _, _, _, _) = symt

putSymTable :: State -> SymTable -> State
putSymTable (flag, _, stack, scope, types, subp) symt = (flag, symt, stack, scope, types, subp)

symTableInsert :: String -> SymbolEntry -> SymTable -> SymTable
symTableInsert name entry [] = [(name, [entry])]
symTableInsert name entry (sym@(name', entry') : symt)
  | name == name' = error "."
  | otherwise = sym : symTableInsert name entry symt

symTableUpdate :: String -> Token -> SymTable -> SymTable
symTableUpdate _ _ [] = error "No variable with given name.\n"
symTableUpdate name val (sym@(name', (mod, t, val') : entry) : symt)
  | name == name' = case mod of
      Mut _ ->
        if typeof val /= typeof t
          then typerror
          else (name', (mod, t, val) : entry) : symt
      Let _ -> error "You can't change an immutable variable.\n"
  | otherwise = sym : symTableUpdate name val symt
  where
    typerror =
      error $
        "Type mismatch at "
          ++ show (pos val)
          ++ ".\n"
          ++ "Expected "
          ++ typeof t
          ++ ", got "
          ++ typeof val
          ++ ".\n"

symTableGetVal :: String -> SymTable -> Token
symTableGetVal _ [] = error "No variable with given name."
symTableGetVal name ((name', (_, _, val) : _) : symt)
  | name == name' = val
  | otherwise = symTableGetVal name symt
