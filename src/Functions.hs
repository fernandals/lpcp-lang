module Functions where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Expressions
import FluxControl
import Lexer
import State
import Text.Parsec hiding (State)
import Tokens
import Types
import Utils

paramDecl :: ParsecT [Token] State IO (Token, Token)
paramDecl = do
  param_name <- idToken
  colonToken
  param_type <- types
  return (param_name, param_type)

returnDecl :: ParsecT [Token] State IO [Token]
returnDecl = do
  arrow <- arrowToken
  return_type <- types
  return [arrow, return_type]

funDecl :: ParsecT [Token] State IO [Token]
funDecl = do
  fun_tk <- funToken
  fun_name <- idToken

  (_, _, (act_name, _) : _, _, _) <- getState
  updateState $ pushStack $ scopeNameBlock act_name (name fun_name)

  lp <- beginpToken
  params <- paramDecl `sepBy` commaToken
  rp <- endpToken
  return_type <- option [] returnDecl
  block <- blockParser

  let entry = (name fun_name, names params, hasReturn return_type, block)

  updateState $ pushSubprogram entry
  updateState popStack

  (_, _, _, _, subp) <- getState
  liftIO $ print subp

  return $ fun_tk : fun_name : lp : toList params ++ rp : return_type ++ block
  where
    names = fmap $ first name
    toList = concatMap (\(x, y) -> [x, y])
    hasReturn [] = Nothing
    hasReturn [_, t] = Just t

-- Now the hard part: semantics!

funCall :: ParsecT [Token] State IO (Token, [Token])
funCall = do
  (flag, _, (act_name, depth) : _, _, subp) <- getState

  f <- idToken
  lp <- beginpToken
  actual_parameters <- expression `sepBy` commaToken
  rp <- endpToken

  let (f_name, params, return_type, code) = getSubp (name f) subp
  let scope_name = scopeNameBlock act_name f_name
  updateState $ pushStack scope_name

  if length params /= length actual_parameters
    then error "faltou parametro"
    else allocateParam actual_parameters params

  (flag, symt, _, _, _) <- getState
  liftIO $ print symt

  updateState $ symTableCleanScope scope_name
  updateState popStack

  return (Eq (0, 0), [])

allocateParam :: [(Token, [Token])] -> [(String, Token)] -> ParsecT [Token] State IO [Token]
allocateParam [] [] = return []
allocateParam ((actual_param, expr) : actual_params) ((formal_param, param_type) : formal_params) = do
  (_, _, (act_name, depth) : stack, _, _) <- getState

  let expected_type = typeof param_type
  let actual_type = typeof actual_param

  when (expected_type /= actual_type) $
    error "tipo errado"

  updateState $ symTableInsert (scopeNameVar act_name formal_param) (Mut (0, 0), param_type, actual_param)

  allocateParam actual_params formal_params
