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
