-- Parsers for state manipulation

{-# LANGUAGE UnicodeSyntax #-}

module State where

import Lexer
import Utils

-- Execution
------------

-- The execution flag indicates whether the program should be running or not.
type ExecutionFlag = Bool

-- Symbol Table
---------------

-- An entry of a variable contains its depth, modifier, type, and value.
type SymbolEntry = (Int, Token, Token, Token)

-- The symbol contains a variable name associated with its entries, so it can support recursion.
type Symbol = (String, [SymbolEntry])

-- The symbol table is a collection of symbols.
type SymTable = [Symbol]

-- Activation Stack
-------------------

-- The activation entry contains the name of the function being executed, plus the depth of its activation.
-- A recursive function has different types of depth.
type ActEntry = (String, Int)

-- The activation stack accompanies the dynamic chain.
-- The top of the activation stack contains the most recent version of a function, its running version.
type ActStack = [ActEntry]

-- Type Table
-------------

-- A type entry is the name of the created type.
-- Needs modification!
type TypeEntry = String

-- The type table is a list of type entries.
type TypeTable = [TypeEntry]

-- Subprograms Table
--------------------

-- A subprogram parameter contains its name and associated token.
type SubpParam = (String, Token)

-- A subprogram may or may not have a return type.
type SubpReturn = Maybe Token

-- A subprogram entry consists of its name, the parameter list, a return section, and the function body.
type SubpEntry = (String, [SubpParam], SubpReturn, [Token])

-- The subprogram table is a list of subprogram entries.
type SubpTable = [SubpEntry]

-- Complete state
-----------------

-- The state is a tuple containing an execution flag, a symbol table, an activation stack, a type table, and a subprogram table.
type State = (ExecutionFlag, SymTable, ActStack, TypeTable, SubpTable)

-- | The initial state.
-- The initial state is basically empty, containing only a global scope activation entry.
defaultState :: State
defaultState = (False, [], [("_global_", 0)], [], [])

-- GETS & SETS for state elements
---------------------------------

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

-- Symbol Table Operations
--------------------------

-- | Inserts a new variable in the symbol table
-- Checks if the variable exists in the current activation.
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
symTableUpdate "" val _ = error "The variable wasn't declared."
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
symTableGetVal "" pos _ = error "Variable not found!"
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

-- Activation Stack Operations
------------------------------

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

-- Subprogram Table Operations
------------------------------

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
    then error "Subprogram already defined."
    else subp : defineSubp entry table

getSubp :: String -> Pos -> SubpTable -> SubpEntry
getSubp "_global_" pos _ = error "Couldn't find subprogram."
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
