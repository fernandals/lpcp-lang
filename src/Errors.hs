-- Functions to deal with recurring errors

module Errors where

import Lexer
import Utils

-- | Occurs when the module keyword is missing in the code.
missingModuleErrorMsg :: String
missingModuleErrorMsg =
  "Your program is missing a module declarations.\n"
    ++ "Please define a `module programName` so your program can run.\n"

-- | Occurs when there is no function with name main.
missingMainErrorMsg :: String
missingMainErrorMsg =
  "Can't execute the code. It is missing a `main` function declaration.\n"
    ++ "Please declare a `fun main() { ... }` so your program can actually run.\n"

-- | Shows error for incompatible type operations.
-- When a token has a different type than expected.
typeErrorMsg :: Pos -> Token -> Token -> String
typeErrorMsg pos expected_type actual_type =
  "Type mismatch at "
    ++ show pos
    ++ ".\nExpected "
    ++ typeof expected_type
    ++ ", got "
    ++ typeof actual_type
    ++ ".\n"

-- | Shows error for incompatible type operations.
-- When an operation receives a token with an incompatible type.
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

-- | Shows error for incompatible type relations.
-- When tokens cannot be compared due to type mismatch.
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

-- | Shows error for incompatible type relations.
-- When a relation receives a token with an incompatible type
typeErrorUnary :: Pos -> String -> Type -> String
typeErrorUnary pos op_name t =
  "Type mismatch at evaluation of expression in "
    ++ show pos
    ++ ".\nCan't apply "
    ++ op_name
    ++ " to "
    ++ typeof' t
    ++ ".\n"

-- | Occurs when encountering an unexpected operator.
unexpectedOperatorError :: Pos -> Token -> String
unexpectedOperatorError pos t =
  "Unexpeted "
    ++ show t
    ++ "on position "
    ++ show pos
    ++ ".\n"
