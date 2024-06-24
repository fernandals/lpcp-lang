module Functions where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
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
