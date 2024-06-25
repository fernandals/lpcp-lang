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

type TypeEntry = String

type TypeTable = [TypeEntry]

type SubpEntry = String

type SubpTable = [SubpEntry]

type State = (ExecutionFlag, SymTable, ActStack, TypeTable, SubpTable)

defaultState :: State
defaultState = (False, [], [("_global_", 0)], [], [])

-- GETS & SETS for state entries

setFlag :: ExecutionFlag -> State -> State
setFlag flag (_, symt, stack, types, subp) = (flag, symt, stack, types, subp)

getFlag :: State -> ExecutionFlag
getFlag (flag, _, _, _, _) = flag

setSymTable :: SymTable -> State -> State
setSymTable symt (flag, _, stack, types, subp) = (flag, symt, stack, types, subp)

getSymTable :: State -> SymTable
getSymTable (_, symt, _, _, _) = symt

setStack :: ActStack -> State -> State
setStack stack (flag, symt, _, types, subp) = (flag, symt, stack, types, subp)

getStack :: State -> ActStack
getStack (_, _, stack, _, _) = stack

setTypes :: TypeTable -> State -> State
setTypes types (flag, symt, stack, _, subp) = (flag, symt, stack, types, subp)

getTypes :: State -> TypeTable
getTypes (_, _, _, types, _) = types

setSubp :: SubpTable -> State -> State
setSubp subp (flag, symt, stack, types, _) = (flag, symt, stack, types, subp)

getSubp :: State -> SubpTable
getSubp (_, _, _, _, subp) = subp

-- ACT STACK OPERATIONS
pushStack :: String -> State -> State
pushStack name (flag, symt, act@(name', depth) : stack, types, subp) =
  if name == name'
    then
      ( flag,
        symt,
        (name, depth + 1) : act : stack,
        types,
        subp
      )
    else
      ( flag,
        symt,
        (name, 0) : act : stack,
        types,
        subp
      )

popStack :: State -> State
popStack (flag, symt, (_, _) : stack, types, subp) = (flag, symt, stack, types, subp)

-- TYPE operations

typeInsert :: TypeEntry -> State -> State
typeInsert name state =
  case getTypes state of 
    [] -> setTypes [name] state
    typ@(x : xs) -> 
      if name == x
        then error "This record already exists and can't be redeclared.\n"
        else setTypes (name : typ) state
  
-- SYMBOL TABLE operations

symTableInsert :: String -> SymbolEntry -> State -> State
symTableInsert name entry state =
  case getSymTable state of
    [] -> setSymTable [(name, [entry])] state
    sym@(name', entry') : symt ->
      if name == name'
        then error "This variable already exists and can't be redeclared.\n" -- missing recursion depth
        else setSymTable (sym : getSymTable insert_state) state
      where
        pop_state = setSymTable symt state
        insert_state = symTableInsert name entry pop_state

symTableUpdate :: String -> Token -> State -> State
symTableUpdate "" val _ = error "Nao achei"
symTableUpdate name val state =
  case symTableFindUpdate name val (getSymTable state) of
    Nothing -> symTableUpdate (parentScopeVar name) val state
    Just symt -> setSymTable symt state

symTableFindUpdate :: String -> Token -> SymTable -> Maybe SymTable
symTableFindUpdate _ _ [] = Nothing
symTableFindUpdate name val (sym@(name', (mod, t, val') : entries) : symt) =
  if name == name'
    then case mod of
      Mut _ ->
        if typeof val /= typeof t
          then typerror
          else Just $ (name', (mod, t, val) : entries) : symt
      Let _ -> error "You can't change an immutable variable.\n"
    else (sym :) <$> symTableFindUpdate name val symt
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

symTableGetVal :: String -> Pos -> State -> Token
symTableGetVal "" pos _ = error "Nao achei"
symTableGetVal name pos state =
  case symTableFindVal name (getSymTable state) of
    Nothing -> symTableGetVal (parentScopeVar name) pos state
    Just val -> val

symTableFindVal :: String -> SymTable -> Maybe Token
symTableFindVal _ [] = Nothing
symTableFindVal name (sym@(name', (_, _, val) : entries) : symt) =
  if name == name'
    then Just val
    else symTableFindVal name symt

symTableCleanScope :: String -> State -> State
symTableCleanScope scope_name (flag, symt, stack, types, subp) =
  ( flag,
    delByScope symt,
    stack,
    types,
    subp
  )
  where
    delByScope = filter (not . inScope scope_name . getName)
    getName (name, _) = name

-- delByScope :: String -> SymTable -> SymTable
-- delByScope _ [] = []
-- delByScope scope_name (sym@(name, entries) : symt) =
--   if inScope scope_name name
--     then delByScope scope_name symt
--     else sym : delByScope scope_name symt
