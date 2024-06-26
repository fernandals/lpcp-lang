{-# LANGUAGE UnicodeSyntax #-}

module State where

import Lexer
import Utils

-- Wether it should execute or just parse
type ExecutionFlag = Bool

-- An entry to a variable in the table contains its DEPTH, MODIFIER, TYPE and VALUE.
-- The symbol table contains a list of its versions, so it can support recursion.
-- The top of the stack of activations contains the most recent version of it, its executing version.
type SymbolEntry = (Int, Token, Token, Token)

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

type SubpEntry = (String, [(String, Token)], Maybe Token, [Token])

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

-- ACT STACK operations

pushStack :: String -> State -> State
pushStack name (flag, symt, act@(name', depth) : stack, types, subp) =
  if name == removeBlockNames name'
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


pushStackBlock :: String -> State -> State
pushStackBlock name (flag, symt, act@(_, depth) : stack, types, subp) 
  = ( flag,
      symt,
      (name, depth) : act : stack,
      types,
      subp
    )

popStack :: State -> State
popStack (flag, symt, (_, _) : stack, types, subp) = (flag, symt, stack, types, subp)

-- SYMBOL TABLE operations

symTableInsert :: String -> SymbolEntry -> State -> State
symTableInsert name entry@(depth, _, _, _) state =
  case getSymTable state of
    [] -> setSymTable [(name, [entry])] state
    sym@(name', entry'@(depth', _, _, _) : entries) : symt ->
      if name == name' -- variable found
        then if depth == depth' -- they're in the same program act
          then error "This variable already exists and can't be redeclared.\n"
          else setSymTable  ((name', entry : entry' : entries) : symt) state
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
symTableFindUpdate name val (sym@(name', (dep, mod, t, val') : entries) : symt) =
  if name == name'
    then case mod of
      Mut _ ->
        if typeof val /= typeof t
          then typerror
          else Just $ (name', (dep, mod, t, val) : entries) : symt
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
symTableGetVal "" pos _ = error "A varivável procurada nao ta nos escopos possiveis!!"
symTableGetVal name pos state =
  case symTableFindVal name (getSymTable state) of
    Nothing -> symTableGetVal (parentScopeVar name) pos state
    Just val -> val

symTableFindVal :: String -> SymTable -> Maybe Token
symTableFindVal _ [] = Nothing
symTableFindVal name (sym@(name', (_, _, _, val) : entries) : symt) =
  if name == name'
    then Just val
    else symTableFindVal name symt

symTableCleanScope :: String -> State -> State
symTableCleanScope scope_name (flag, symt, stack, types, subp) =
  ( flag,
    removeEmpty . delByScope scope_name $ symt,
    stack,
    types,
    subp
  )

delByScope :: String -> SymTable -> SymTable
delByScope scope_name [] = []
delByScope scope_name (sym@(name', (_, _, _, _) : entries) : symt) =
  if inScope scope_name name'
    then (name', entries) : symt
    else sym : delByScope scope_name symt
         
removeEmpty :: SymTable -> SymTable
removeEmpty [] = []
removeEmpty (sym@(_, []) : symt) = removeEmpty symt
removeEmpty (sym@(_, entries) : symt) = sym : removeEmpty symt 


pushSubprogram :: SubpEntry -> State -> State
pushSubprogram entry (flag, symt, stack, types, subp) =
  ( flag,
    symt,
    stack,
    types,
    defineSubp entry subp
  )

defineSubp :: SubpEntry -> SubpTable -> SubpTable
defineSubp entry [] = [entry]
defineSubp entry@(fun_name, params, return_type, block) (subp@(fun_name', _, _, _) : table) =
  if fun_name == fun_name'
    then error "already defined"
    else subp : defineSubp entry table

getSubp :: String -> Pos -> SubpTable -> SubpEntry
getSubp "_global_" pos _ = error "nao achei o subp"
getSubp name pos subp = case findSubp name subp of
  Nothing -> getSubp (parentScopeBlock name) pos subp
  Just sub -> sub

findSubp :: String -> SubpTable -> Maybe SubpEntry
findSubp _ [] = Nothing
findSubp name (entry@(name', _, _, _) : subp) =
  if name == name'
    then Just entry
    else findSubp name subp

returnSubp :: String -> Maybe Token -> State -> State
returnSubp f_name return_val (flag, symt, stack, types, subp) =
  ( flag,
    symt,
    stack,
    types,
    updateReturnSubp f_name return_val subp
  )

updateReturnSubp :: String -> Maybe Token -> SubpTable -> SubpTable
updateReturnSubp _ _ [] = []
updateReturnSubp f_name return_val (entry@(name, params, return_type, code) : subp) =
  if f_name == name
    then (name, params, return_val, code) : subp
    else entry : updateReturnSubp f_name return_val subp
