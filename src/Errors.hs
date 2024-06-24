module Errors where

import Lexer
import Utils

missingModuleErrorMsg :: String
missingModuleErrorMsg =
  "Your program is missing a module declarations.\n"
    ++ "Please define a `module programName` so your program can run.\n"

missingMainErrorMsg :: String
missingMainErrorMsg =
  "Can't execute the code. It is missing a `main` function declaration.\n"
    ++ "Please declare a `fun main() { ... }` so your program can actually run.\n"

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
  "Type mismatch at evaluation of expression at "
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
  "Type mismatch at evaluation of expression at "
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
  "Type mismatch at evaluation of expression at "
    ++ show pos
    ++ ".\nCan't apply "
    ++ op_name
    ++ " to "
    ++ typeof' t
    ++ ".\n"

unexpectedOperatorError :: Pos -> Token -> String
unexpectedOperatorError pos t =
  "Unexpected "
    ++ show t
    ++ "at position "
    ++ show pos
    ++ ".\n"

nonIntegerIndex :: Pos -> String
nonIntegerIndex p =
  "Non integer index at "
    ++ show p
    ++ ".\n"

outOfBounds :: Pos -> Int -> String
outOfBounds p len =
  "Index out of bounds at "
    ++ show p
    ++ ".\nSize of list is "
    ++ show len
    ++ ".\n"

indexInNonList :: Pos -> Token -> String
indexInNonList p t =
  "Illegal indexing at "
    ++ show p
    ++ ".\nAn "
    ++ typeof t
    ++ " isn't subscriptable.\n"

nonHomogeneousList :: Pos -> Type -> Type -> String
nonHomogeneousList p t t' =
  "Non homogeneous list at "
    ++ show p
    ++ ".\n"
    ++ "Expected "
    ++ typeof' t
    ++ ", got "
    ++ typeof' t'
    ++ ".\n"
