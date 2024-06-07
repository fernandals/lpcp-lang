{-# LANGUAGE UnicodeSyntax #-}

module State where

import Lexer

-- modifier, type, id, val
type StateEntry = (Token, Token, Token, Token)

type StateQuery = (Token, Token)

type State = [StateEntry]

stateInsert :: StateEntry -> State -> State
stateInsert entry [] = [entry]
stateInsert entry@(mod, t, id, val) (sym@(_, _, id', val') : σ)
  | id == id' = entry : σ
  | otherwise = sym : stateInsert entry σ

stateUpdate :: StateQuery -> State -> State
stateUpdate _ [] = error "No variable with given name."
stateUpdate query@(id, val) (sym@(mod', t', id', val') : σ)
  | id == id' = case mod' of
      Mut _ -> (mod', t', id', val) : σ
      Let _ -> error "You can't change an immutable value."
  | otherwise = sym : stateUpdate query σ

stateDelete :: StateQuery -> State -> State
stateDelete _ [] = error "No variable with given name."
stateDelete query@(id, val) (sym@(_, _, id', val') : σ)
  | id == id' = σ
  | otherwise = sym : stateDelete query σ

getValue :: Token -> State -> Token
getValue _ [] = error "No variable with given name."
getValue id (sym@(mod', t', id', val') : σ)
  | id == id' = val'