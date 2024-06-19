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

defaultState :: State
defaultState = (False, [], [("_global_", 0)], [], [], [])

-- GETS & SETS for state entries

setFlag :: ExecutionFlag -> State -> State
setFlag flag (_, symt, stack, scope, types, subp) = (flag, symt, stack, scope, types, subp)

getFlag :: State -> ExecutionFlag 
getFlag (flag, _, _, _, _, _) = flag

setSymTable :: SymTable -> State -> State
setSymTable symt (flag, _, stack, scope, types, subp) = (flag, symt, stack, scope, types, subp) 

getSymTable :: State -> SymTable
getSymTable (_, symt, _, _, _, _) = symt

setStack :: ActStack -> State -> State
setStack stack (flag, symt, _, scope, types, subp) = (flag, symt, stack, scope, types, subp) 

getStack :: State -> ActStack
getStack (_, _, stack, _, _, _) = stack

setScope :: ScopeTable -> State -> State
setScope scope (flag, symt, stack, _, types, subp) = (flag, symt, stack, scope, types, subp) 

getScope :: State -> ScopeTable
getScope (_, _, _, scope, _, _) = scope 

setTypes :: TypeTable -> State -> State
setTypes types (flag, symt, stack, scope, _, subp) = (flag, symt, stack, scope, types, subp) 

getTypes :: State -> TypeTable
getTypes (_, _, _, _, types, _) = types

setSubp :: SubpTable -> State -> State
setSubp subp (flag, symt, stack, scope, types, _) = (flag, symt, stack, scope, types, subp) 

getSubp :: State -> SubpTable
getSubp (_, _, _, _, _, subp) = subp 

-- outdated!
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

symTableGetVal :: String -> String -> SymTable -> Token
symTableGetVal name act [] = getValByScope name act
symTableGetVal name act ((name', (_, _, val) : _) : symt)
  | name == name' = val
  | otherwise = symTableGetVal name act symt

getValByScope :: String -> String -> SymTable -> Token

pushIntoStack :: String -> ActStack -> ActStack
pushIntoStack name [] = [(name, 0)]
pushIntoStack name (act@(name', depth) : stack)
  | name == name' = (name, depth + 1) : act : stack
  | otherwise = act : pushIntoStack name stack
