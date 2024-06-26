{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import Builtin
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.List (intersperse)
import Data.Maybe (isNothing, maybe)
import Errors
import ExpressionsEvaluation
import Lexer
import State
import System.IO (hFlush, stdout)
import Text.Parsec hiding (State)
import Text.Read (get, lift, readMaybe)
import Tokens
import Utils

-- Types involved in declarations

referenceType :: ParsecT [Token] State IO Token
referenceType = do
  amper <- amperToken
  t <- types

  when (isRef t) $ error "AA"

  return $ Reference (pos amper) t
  where
    isRef (Reference _ _) = True
    isRef _ = False

types :: ParsecT [Token] State IO Token
types = try primTypes <|> compTypes

primTypes :: ParsecT [Token] State IO Token
primTypes = intToken <|> floatToken <|> boolToken <|> charToken <|> stringToken

compTypes :: ParsecT [Token] State IO Token
compTypes = listType <|>  referenceType 

listType :: ParsecT [Token] State IO Token
listType = do
  l <- beginSBToken
  t <- types
  r <- endSBToken
  return (List (pos l) t)

-- Main program
---------------

parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program defaultState "Error"

program :: ParsecT [Token] State IO [Token]
program = do
  p <- moduleToken <?> missingModuleErrorMsg
  pn <- idToken <?> missingModuleErrorMsg
  d <- many (try funDecl <|> varDecl)
  main <- mainProgram <?> missingMainErrorMsg
  eof
  return $ [p, pn] ++ concat d ++ main

mainProgram :: ParsecT [Token] State IO [Token]
mainProgram = do
  fun <- funToken
  main <- mainFun
  beginpToken >> endpToken

  let scope_name = scopeNameBlock "_global_" "main"

  updateState $ setFlag True
  updateState $ pushStackBlock scope_name

  block <- blockParser

  return $ main : block

-- Program statements
---------------------

exprComma :: ParsecT [Token] State IO (Token, [Token])
exprComma =
  try
    ( do
        (v, expr) <- expression
        comma <- commaToken

        return (v, expr ++ [comma])
    )
    <|> ( do
            expression
        )

exprCommaSt :: ParsecT [Token] State IO [Token]
exprCommaSt =
  try
    ( do
        expr <- binExpr
        comma <- commaToken

        return $ expr ++ [comma]
    )
    <|> (do binExpr)

statements :: ParsecT [Token] State IO [Token]
statements = try funCallSt  <|> try assignIndex <|> try prependStmt <|> try appendStmt <|> assignSt <|> printSt <|> printfSt

assignSt :: ParsecT [Token] State IO [Token]
assignSt = do
  id <- idToken
  assign <- assignToken

  state@(flag, symt, (act_name, _) : stack, types, subp) <- getState
  if canExecute flag act_name
    then do
      (v', expr) <- getSt <|> expression

      let l_name = name id
      let l_p = pos id
      let actual_value = symTableGetVal (scopeNameVar act_name l_name) l_p state
      let emp = (isEmpty v') && (isListValue actual_value) 
      let v = if emp then case actual_value of
            LiteralValue p (L t i l) -> changeTypeOfList v' (typeAsToken (L t i l) p)
            x -> error $ "Error: Object at position" ++ show (pos x) ++ " is not a list."
          else v'


      updateState $ symTableUpdate (scopeNameVar act_name (name id)) v
      return $ id : assign : expr
    else do
      expr <- getStSyntactic <|> binExpr
      return $ id : assign : expr

printSt :: ParsecT [Token] State IO [Token]
printSt =
  do
    comm <- printFun <|> printLnFun

    (flag, _, (act_name, _) : _, _, _) <- getState
    if canExecute flag act_name
      then do
        lp <- beginpToken
        (v, expr) <- expression
        liftIO
          $ case comm of
            (Print _) -> putStr . show
            (PrintLn _) -> print
          $ v
        liftIO $ hFlush stdout
        rp <- endpToken
        return $ comm : lp : expr ++ [rp]
      else do
        lp <- beginpToken
        expr <- binExpr
        rp <- endpToken
        return $ comm : lp : expr ++ [rp]

printfSt :: ParsecT [Token] State IO [Token]
printfSt =
  do
    comm <- printFFun

    (flag, _, (act_name, _) : _, _, _) <- getState
    if canExecute flag act_name
      then do
        lp <- beginpToken
        args <- many1 exprComma
        rp <- endpToken

        liftIO $
          putStrLn $
            foldr1 (++) (show . fst <$> args)

        return $ comm : lp : concatMap snd args ++ [rp]
      else do
        lp <- beginpToken
        args <- many1 exprCommaSt
        rp <- endpToken
        return $ comm : lp : concat args ++ [rp]

-- Input Statements
-- getInt() | getFloat | getChar | getString
getSt :: ParsecT [Token] State IO (Token, [Token])
getSt = do
  comm <- getIntFun <|> getFloatFun <|> getCharFun <|> getStringFun
  lp <- beginpToken
  rp <- endpToken

  input <- liftIO getLine

  return $ (,[comm, lp, rp]) $ case comm of
    (GetInt p) -> LiteralValue p (I $ parseInput p input)
    (GetFloat p) -> LiteralValue p (F $ parseInput p input)
    (GetChar p) -> LiteralValue p (C $ parseInput p input)
    (GetString p) -> LiteralValue p (S input)

getStSyntactic :: ParsecT [Token] State IO [Token]
getStSyntactic = do
  comm <- getIntFun <|> getFloatFun <|> getCharFun <|> getStringFun
  lp <- beginpToken
  rp <- endpToken

  return [comm, lp, rp]

parseInput :: (Read a) => Pos -> String -> a
parseInput p = check p . readMaybe
  where
    check p (Just x) = x
    check p Nothing = error $ "Couldn't parse input at " ++ show p ++ ". Maybe the type doesn't match?\n"

blockParser :: ParsecT [Token] State IO [Token]
blockParser = do
  begin <- beginBToken

  lines <- many (try decls <|> try statements <|> try ifParser <|> try whileParser <|> returnSt)

  end <- endBToken

  return $ begin : concat lines ++ [end]

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
  let scope_name = scopeNameBlock act_name (name fun_name)
  updateState $ pushStack scope_name

  lp <- beginpToken
  params <- paramDecl `sepBy` commaToken
  rp <- endpToken
  return_type <- option [] returnDecl
  block <- blockParser

  let entry = (scope_name, names params, hasReturn return_type, block)

  updateState $ pushSubprogram entry
  updateState popStack

  return $ fun_tk : fun_name : lp : toList params ++ rp : return_type ++ block
  where
    names = fmap $ first name
    toList = concatMap (\(x, y) -> [x, y])
    hasReturn [] = Nothing
    hasReturn [_, t] = Just t

decls :: ParsecT [Token] State IO [Token]
decls = do
  varDecl

varDecl :: ParsecT [Token] State IO [Token]
varDecl = do
  modifier <- letToken <|> mutToken
  id <- idToken
  colon <- colonToken
  decltype <- types
  assign <- assignToken

  (flag, _, (act_name, depth) : _, _, _) <- getState

  if canExecute flag act_name
    then do
      (v', expr) <- getSt <|> expression
      let expected_type = typeof decltype
      let v = v'
      let emp = (isEmpty v') && (isList decltype) 
      let actual_type = if emp then typeof decltype else typeof v'
      let v = if emp then changeTypeOfList v' decltype else v'

      when (expected_type /= actual_type) $
        error $
          typeErrorMsg (pos id) decltype v

      updateState $ symTableInsert (scopeNameVar act_name (name id)) (depth, modifier, decltype, v)

      return $ modifier : id : colon : decltype : assign : expr
    else do
      expr <- getStSyntactic <|> binExpr
      return $
        [ modifier,
          id,
          colon,
          decltype,
          assign
        ]
          ++ expr

-- | List stmts

assignIndex :: ParsecT [Token] State IO [Token]
assignIndex = do
  list_name <- idToken
  sbl <- beginSBToken
  (index, exprIndex) <- getSt <|> expression
  sbr <- endSBToken
  ids <- many many_indexs
  assign <- assignToken

  let l_name = name list_name
  let l_p = pos list_name
  
  let allIdExprs = exprIndex ++ (concatMap snd ids)

  let listIds = index : map fst ids

  state@(flag, symt, (act_name, _) : stack, types, subp) <- getState

  if canExecute flag act_name
    then do
      let val = symTableGetVal (scopeNameVar act_name l_name) l_p state

      (v, expr) <- getSt <|> expression
      v' <- tokenToType (v, expr)

      let newVal =  setMat listIds val v -- canInsert val index v'
      updateState $ symTableUpdate (scopeNameVar act_name (name list_name)) newVal
      return $ list_name : sbl : exprIndex ++ [sbr] ++ allIdExprs ++ [assign] ++ expr
    else do
      expr <- getStSyntactic <|> binExpr
      return $ list_name : sbl : exprIndex ++ [sbr] ++ allIdExprs ++ [assign] ++ expr


appendStmt :: ParsecT [Token] State IO [Token]
appendStmt = do 
  list_name <- idToken
  ids <- many many_indexs
  append <- appendFun
  (e, expr) <- getSt <|> expression

  let l_name = name list_name
  let l_p = pos list_name
  let type_e = typeof e 

  let allIdExprs = concatMap snd ids
  let listIds = map fst ids

  state@(flag, symt, (act_name, _) : stack, types, subp) <- getState

  if canExecute flag act_name
    then do

      let val = symTableGetVal (scopeNameVar act_name l_name) l_p state
      let newVal = if listIds == [] then appendAux e val else appendMany listIds val e

      updateState $ symTableUpdate (scopeNameVar act_name (name list_name)) newVal
      return $ [list_name]  ++ allIdExprs ++ [append] ++ expr

    else
      return $ [list_name]  ++ allIdExprs ++ [append] ++ expr


prependStmt :: ParsecT [Token] State IO [Token]
prependStmt = do 
  (e, expr) <- getSt <|> expression
  prepend <- prependFun
  list_name <- idToken
  ids <- many many_indexs

  let l_name = name list_name
  let l_p = pos list_name
  let type_e = typeof e 


  let allIdExprs = concatMap snd ids
  let listIds = map fst ids

  state@(flag, symt, (act_name, _) : stack, types, subp) <- getState

  if canExecute flag act_name
    then do

      let val = symTableGetVal (scopeNameVar act_name l_name) l_p state
      let newVal = if listIds == [] then prependAux e val else prependMany listIds val e

      updateState $ symTableUpdate (scopeNameVar act_name (name list_name)) newVal
      return $ expr ++ [prepend] ++ allIdExprs ++ [list_name]

    else
      return $ expr ++ [prepend] ++ allIdExprs ++ [list_name]

-- | Aux to list stmts
-----------------------------------

changeTypeOfList :: Token -> Token -> Token
changeTypeOfList (LiteralValue p (L t i l)) (List p' t') = (LiteralValue p (L t' i l))
changeTypeOfList x _ = error $ "Error: Object at position " ++ show (pos x) ++ " is not a list."

isEmpty :: Token -> Bool
isEmpty (LiteralValue p (L (EmptyList p') i l)) = True 
isEmpty _ = False

isList  :: Token -> Bool
isList (List p' t) = True
isList _ = False

isListValue :: Token -> Bool
isListValue (LiteralValue p (L t i l)) = True
isListValue _ = False

typeOfElement :: String -> String
typeOfElement str = if length str <= 2
                         then ""
                         else tail (init str)

-- prepend and append

prependAux :: Token -> Token -> Token
prependAux (LiteralValue p' x) (LiteralValue p (L t i l)) = 
  if (typeof t) == (typeof' x)
    then LiteralValue p (L t (i+1) (x:l))
    else error $ typeErrorMsg p' t (typeAsToken x p') 
prependAux (LiteralValue p' x) _ = error $ "Error: Is not a list at " ++ show p'
prependAux x y = error $ "Error: Invalid prepend stmt at " ++ show (pos x)

prependMany :: [Token] -> Token -> Token -> Token
prependMany [] l v = prependAux v l
prependMany (i:is) l v = setList' i l (prependMany is (getList i l) v)

appendAux :: Token -> Token -> Token
appendAux (LiteralValue p' x) (LiteralValue p (L t i l)) = 
  if (typeof t) == (typeof' x)
    then LiteralValue p (L t (i+1) (l++[x]))
    else error $ error $ typeErrorMsg p' t (typeAsToken x p') 
appendAux (LiteralValue p' x) _ = error $ "Error: Is not a list at " ++ show p'
appendAux x y = error $ "Error: Invalid append stmt at " ++ show (pos x)

appendMany :: [Token] -> Token -> Token -> Token
appendMany [] l v = appendAux v l
appendMany (i:is) l v = setList' i l (appendMany is (getList i l) v)

-- assign with index

setMat :: [Token] -> Token -> Token -> Token
setMat (i:is) (LiteralValue p (L t len [])) v = error $ "Error: Attempt to access an index in an empty matrix or list at position " ++ show (pos i)
setMat (i:is) (LiteralValue p (L t len (x:xs))) v 
  | null is = setList' i (LiteralValue p (L t len (x:xs))) v
  | otherwise = 
    case x of 
      L t' len ls -> setList' i (LiteralValue p (L t len (x:xs))) (setMat is (getList i (LiteralValue p (L t len (x:xs)))) v)
      _ -> error $ outOfBounds p len

setList' :: Token -> Token -> Token -> Token
setList' (LiteralValue p' (I i)) (LiteralValue p (L t len l)) value 
  | i < 0          = error $ "Error: Negative index at position " ++ show p' 
  | i > len        = error $ outOfBounds p len
  | otherwise      = LiteralValue p (L t len (go i l))
  where
    go 0 (x:xs) = if typeof' x == typeof' (tokenToType' value)
      then (tokenToType' value) : xs
      else error $ "Error: Type mismatch at " ++ show (pos value)
    go n (x: xs) = x : go (n-1) xs
    go n [] = error $ outOfBounds p' len
setList' y  (LiteralValue p (L t len l)) value = error $ nonIntegerIndex (pos y)
setList' _ y value  = error $ "Error: Attempt to insert an element non-list object"

getList :: Token -> Token -> Token
getList (LiteralValue p' (I i)) (LiteralValue p (L t len l)) =
      if i < length l
        then LiteralValue p $ l !! i
        else error $ outOfBounds p' len
getList x _ = error $ nonIntegerIndex (pos x)

tokenToType' :: Token -> Type
tokenToType' (LiteralValue p (I i)) =  I i
tokenToType' (LiteralValue p (F f)) = F f
tokenToType' (LiteralValue p (C c)) =  C c
tokenToType' (LiteralValue p (S s)) = S s
tokenToType' (LiteralValue p (L t i l)) = L t i l

-- | If parsers
---------------------------------------------

ifParser :: ParsecT [Token] State IO [Token]
ifParser = do
  (flag, _, _, _, _) <- getState

  if flag
    then do ifSt
    else do
      if_tk <- ifToken
      expr <- binExpr
      block <- blockParser

      elif_st <- many elifParser
      else_st <- option [] elseParser

      return $ if_tk : expr ++ block ++ concat elif_st ++ else_st

elifParser :: ParsecT [Token] State IO [Token]
elifParser = do
  elif_tk <- elifToken
  expr <- binExpr
  block <- blockParser

  return $ elif_tk : expr ++ block

elseParser :: ParsecT [Token] State IO [Token]
elseParser = do
  else_tk <- elseToken
  block <- blockParser

  return $ else_tk : block

whileParser :: ParsecT [Token] State IO [Token]
whileParser = do
  (flag, _, _, _, _) <- getState
  if flag
    then do whileSt
    else do
      while_tk <- whileToken
      expr <- binExpr
      do_tk <- doToken
      block <- blockParser

      return $ while_tk : expr ++ do_tk : block

-- Semantics
ifSt :: ParsecT [Token] State IO [Token]
ifSt = do
  if_tk <- ifToken
  (v, expr) <- expression

  if isTrue v
    then do
      (_, _, (act_name, _) : _, _, _) <- getState
      let scope_name = scopeNameBlock act_name "if"
      updateState $ pushStackBlock scope_name
      block <- blockParser
      updateState $ symTableCleanScope scope_name
      updateState popStack

      updateState $ setFlag False
      elif_st <- many elifParser
      else_st <- option [] elseParser

      updateState $ setFlag True

      return $ if_tk : expr ++ block ++ concat elif_st ++ else_st
    else do
      updateState $ setFlag False
      block <- blockParser
      updateState $ setFlag True
      next_st <- try elifSt <|> option [] elseSt

      return $ if_tk : expr ++ block ++ next_st

elifSt :: ParsecT [Token] State IO [Token]
elifSt = do
  elif_tk <- elifToken
  (v, expr) <- expression

  if isTrue v
    then do
      (_, _, (act_name, _) : _, _, _) <- getState
      let scope_name = scopeNameBlock act_name "elif"
      updateState $ pushStackBlock scope_name
      block <- blockParser
      updateState $ symTableCleanScope scope_name
      updateState popStack

      updateState $ setFlag False
      elif_st <- many elifParser
      else_st <- option [] elseParser
      updateState $ setFlag True

      return $ elif_tk : expr ++ block ++ concat elif_st ++ else_st
    else do
      updateState $ setFlag False
      block <- blockParser
      updateState $ setFlag True
      next_st <- try elifSt <|> option [] elseSt

      return $ elif_tk : expr ++ block ++ next_st

elseSt :: ParsecT [Token] State IO [Token]
elseSt = do
  else_tk <- elseToken
  (_, _, (act_name, _) : _, _, _) <- getState
  let scope_name = scopeNameBlock act_name "else"
  updateState $ pushStackBlock scope_name
  block <- blockParser
  updateState $ symTableCleanScope scope_name
  updateState popStack
  return $ else_tk : block

whileSt :: ParsecT [Token] State IO [Token]
whileSt = do
  fixp <- getInput
  while_tk <- whileToken
  (v, expr) <- expression
  do_tk <- doToken

  (_, _, (act_name, _) : _, _, _) <- getState
  if isTrue v
    then do
      let scope_name = scopeNameBlock act_name "while"
      updateState $ pushStackBlock scope_name
      block <- blockParser
      updateState $ symTableCleanScope scope_name
      updateState popStack
      setInput fixp
      return $ while_tk : expr ++ do_tk : block
    else do
      updateState $ setFlag False
      block <- blockParser
      updateState $ setFlag True
      return $ while_tk : expr ++ do_tk : block

funCallSt :: ParsecT [Token] State IO [Token]
funCallSt = do
  f <- idToken
  lp <- beginpToken

  (flag, _, (act_name, depth) : _, _, subp) <- getState

  if not flag
    then do
      actual_parameters <- many exprCommaSt
      rp <- endpToken

      return $ f : lp : concat actual_parameters ++ [rp]
    else do
      actual_parameters <- many exprComma
      rp <- endpToken

      let scoped_name = functionName (name f)
      let (f_name, params, return_type, code) = getSubp scoped_name (pos f) subp

      updateState $ pushStack f_name

      if length params /= length actual_parameters
        then error "There are missing parameters!"
        else allocateParam actual_parameters params

      calle <- getInput
      setInput code
      block <- blockParser
      setInput calle

      (flag, _, (act_name, depth) : _, _, subp) <- getState
      let (f_name, params, return_type', code) = getSubp scoped_name (pos f) subp

      if return_type /= return_type'
        then do
          updateState $ returnSubp f_name return_type
          updateState $ setFlag True
          updateState $ symTableCleanScope f_name
          updateState popStack
          return $ f : lp : concatMap snd actual_parameters ++ [rp]
        else do
          updateState $ symTableCleanScope f_name
          updateState popStack
          return $ f : lp : concatMap snd actual_parameters ++ [rp]

-- Expressions Parser

binExpr :: ParsecT [Token] State IO [Token]
binExpr = do
  n1 <- atom <|> unaExpr <|> bracktExpr
  binExprRemaining n1

binExprRemaining :: [Token] -> ParsecT [Token] State IO [Token]
binExprRemaining n1 =
  ( do
      op <- binOp
      n2 <- binExpr
      return (n1 ++ [op] ++ n2)
  )
    <|> return n1

unaExpr :: ParsecT [Token] State IO [Token]
unaExpr = do
  op <- minusToken <|> notToken
  n1 <- atom
  return (op : n1)

binOp :: ParsecT [Token] State IO Token
binOp = do
  plusToken
    <|> minusToken
    <|> timesToken
    <|> dividesToken
    <|> modulosToken
    <|> powToken
    <|> eqToken
    <|> neqToken
    <|> leqToken
    <|> geqToken
    <|> greaterToken
    <|> lessToken
    <|> orToken
    <|> xorToken
    <|> andToken

atom :: ParsecT [Token] State IO [Token]
atom =
  try
    ( do
        fmap snd funCall
    )
    <|> try ( do 
          fmap snd listIndex
        )
        <|> try ( do 
              fmap snd list
            )
            <|>
            ( do
                  a <- idToken <|> literalValueToken
                  return [a]
            )

-- ALTERAR AQUIIII

bracktExpr :: ParsecT [Token] State IO [Token]
bracktExpr = do
  l <- beginpToken
  exp <- try binExpr <|> unaExpr
  r <- endpToken
  return ([l] ++ exp ++ [r])

-- Expresions File

expression :: ParsecT [Token] State IO (Token, [Token])
expression = term >>= expressionRemaining

expressionRemaining :: (Token, [Token]) -> ParsecT [Token] State IO (Token, [Token])
expressionRemaining n1 =
  ( do
      op <- orToken <|> xorToken
      n2 <- term
      expressionRemaining $ eval n1 op n2
  )
    <|> return n1

term :: ParsecT [Token] State IO (Token, [Token])
term = notFactor >>= termRemaining

termRemaining :: (Token, [Token]) -> ParsecT [Token] State IO (Token, [Token])
termRemaining n1 =
  ( do
      op <- andToken
      n2 <- notFactor
      termRemaining $ eval n1 op n2
  )
    <|> return n1

notFactor :: ParsecT [Token] State IO (Token, [Token])
notFactor = try unaBoolExpr <|> factor

factor :: ParsecT [Token] State IO (Token, [Token])
factor = subExpression >>= factorRemaining

factorRemaining :: (Token, [Token]) -> ParsecT [Token] State IO (Token, [Token])
factorRemaining n1 =
  ( do
      rel <- leqToken <|> geqToken <|> lessToken <|> greaterToken <|> eqToken <|> neqToken
      n2 <- subExpression
      factorRemaining $ eval n1 rel n2
  )
    <|> return n1

subExpression :: ParsecT [Token] State IO (Token, [Token])
subExpression = subTerm >>= evalSubExpressionRemaining

evalSubExpressionRemaining :: (Token, [Token]) -> ParsecT [Token] State IO (Token, [Token])
evalSubExpressionRemaining n1 =
  ( do
      op <- plusToken <|> minusToken
      n2 <- subTerm
      evalSubExpressionRemaining (eval n1 op n2)
  )
    <|> return n1

subTerm :: ParsecT [Token] State IO (Token, [Token])
subTerm = negSubFactor >>= evalSubTermRemaining

evalSubTermRemaining :: (Token, [Token]) -> ParsecT [Token] State IO (Token, [Token])
evalSubTermRemaining n1 =
  ( do
      op <- timesToken <|> dividesToken <|> modulosToken
      n2 <- negSubFactor
      evalSubTermRemaining (eval n1 op n2)
  )
    <|> return n1

negSubFactor :: ParsecT [Token] State IO (Token, [Token])
negSubFactor = try unaArithExpr <|> subFactor

subFactor :: ParsecT [Token] State IO (Token, [Token])
subFactor = base >>= evalSubFactorRemaining

evalSubFactorRemaining :: (Token, [Token]) -> ParsecT [Token] State IO (Token, [Token])
evalSubFactorRemaining n1 =
  ( do
      op <- powToken
      n2 <- base
      evalSubFactorRemaining (eval n1 op n2)
  )
    <|> return n1

base :: ParsecT [Token] State IO (Token, [Token])
base = bracketExpression <|> atomExpression

bracketExpression :: ParsecT [Token] State IO (Token, [Token])
bracketExpression = do
  l <- beginpToken
  (v, expr) <- expression
  r <- endpToken
  return (v, l : expr ++ [r])

literalExpression :: ParsecT [Token] State IO (Token, [Token])
literalExpression = do
  n <- literalValueToken
  return (n, [n])

idExpression :: ParsecT [Token] State IO (Token, [Token])
idExpression = do
  n <- idToken
  return (n, [n])

atomExpression :: ParsecT [Token] State IO (Token, [Token])
atomExpression = do
  n <-  try funCall <|> try literalExpression  <|> try listIndex <|> try idExpression <|> try list <|> convToFloat <|> convAbs <|> lengthIter
  evalVar n

-- List as expressions
------------------------

listIndex :: ParsecT [Token] State IO (Token, [Token])
listIndex = do
  list_name <- idToken
  sbl <- beginSBToken
  (index, expr) <- expression
  sbr <- endSBToken

  state@(_, _, (act_name, i) : _, _, _) <- getState

  let l_p = pos list_name
  let l_name = name list_name
  let val = symTableGetVal (scopeNameVar act_name l_name) l_p state

  ids <- many many_indexs

  return $ case index of  
    LiteralValue p (I i) -> (eval_ids val (index: map fst ids), sbl : expr ++ concatMap snd ids ++ [sbr])
    _ -> error $ nonIntegerIndex l_p

eval_ids :: Token -> [Token] -> Token
eval_ids val [] = val
eval_ids val (x:xs) = case x of  
    LiteralValue p (I i) -> eval_ids (indexing val p i) xs
    x -> error $ nonIntegerIndex (pos x)

many_indexs :: ParsecT [Token] State IO (Token, [Token])
many_indexs = do
  sbl <- beginSBToken
  (index, expr) <- expression
  sbr <- endSBToken
  return $ (index, sbl : expr ++ [sbr])

indexing :: Token -> (Int,Int) -> Int -> Token
indexing (LiteralValue p (L t len l)) l_p i =
      if i < len
        then LiteralValue p $ l !! i
        else error $ outOfBounds l_p len
indexing x l_p _ = error $ indexInNonList l_p x


list :: ParsecT [Token] State IO (Token, [Token])
list = do
  l <- beginSBToken
  elements <- expression `sepBy` commaToken
  r <- endSBToken

  lis <- tokensToTypes elements

  let l_p = pos l
      l_type = tokenTypeOfList lis l_p
      l_len = length elements
      comma = Comma (0, 0)
      tokens = intersperse comma $ concatMap snd elements ++ [r]
   in return (LiteralValue l_p (L l_type l_len lis), tokens)

tokensToTypes :: [(Token, [Token])] -> ParsecT [Token] State IO [Type]
tokensToTypes [] = return []
tokensToTypes (x : xs) = do
  t <- tokenToType x
  ts <- tokensToTypes xs
  return (t : ts)

tokenToType :: (Token, [Token]) -> ParsecT [Token] State IO Type
tokenToType x = do
  case fst x of
    LiteralValue p (I i) -> return $ I i
    LiteralValue p (F f) -> return $ F f
    LiteralValue p (C c) -> return $ C c
    LiteralValue p (S s) -> return $ S s
    LiteralValue p (L t i l) -> return $ L t i l

lengthIter :: ParsecT [Token] State IO (Token, [Token])
lengthIter = do
  fun <- lengthFun
  pl <- beginpToken
  (v, expr) <- expression
  pr <- endpToken
  case v of
    LiteralValue p (L t len l) -> return (LiteralValue p (I len), fun : pl : expr ++ [pr])
    LiteralValue p (S s) -> return (LiteralValue p (I (length s)), fun : pl : expr ++ [pr])
    _ -> error $ "Type mismatch at " ++ (show (pos pl)) ++ "."

-- faz nova passada trocando empty pra algum que nao ta empty [1,2]
tokenTypeOfList :: [Type] -> Pos -> Token
tokenTypeOfList [] p = EmptyList p
tokenTypeOfList [x] p = typeAsToken x p
tokenTypeOfList (x : y : xs) p =
  if typeof' x == typeof' y
    then tokenTypeOfList (y : xs) p
    else error $ nonHomogeneousList p x y


-- Functions


convToFloat :: ParsecT [Token] State IO (Token, [Token])
convToFloat = do
  fun <- toFloatFun
  l <- beginpToken
  (v, expr) <- expression
  r <- endpToken
  (nEvaluated, expr) <- evalVar (v, expr)
  case val nEvaluated of
    (I i) -> return (LiteralValue (pos v) $ F (fromIntegral i), fun : l : expr ++ [r])
    t -> error $ typeErrorUnary (pos v) "(toFloat)" t

convToStr :: ParsecT [Token] State IO (Token, [Token])
convToStr = do
  fun <- toStrFun
  l <- beginpToken
  (v, expr) <- expression
  r <- endpToken
  return (LiteralValue (pos v) (S $ show (val v)), fun : l : expr ++ [r])

convAbs :: ParsecT [Token] State IO (Token, [Token])
convAbs = do
  fun <- absFun
  l <- beginpToken
  (v, expr) <- expression
  r <- endpToken
  return (LiteralValue (pos v) (to_abs (pos v) $ val v), fun : l : expr ++ [r])
  where
    to_abs p (I i) = I (abs i)
    to_abs p (F f) = F (abs f)
    to_abs p t = error $ typeErrorUnary p "(abs)" t

negValue :: (Token, [Token]) -> (Token, [Token])
negValue (LiteralValue p a, expr) =
  case a of
    B b -> (LiteralValue p (B $ not b), expr)
    I i -> (LiteralValue p (I (-i)), expr)
    F f -> (LiteralValue p (F (-f)), expr)

unaBoolExpr :: ParsecT [Token] State IO (Token, [Token])
unaBoolExpr = do
  op <- notToken
  b <- expression

  state@(_, _, (act_name, _) : stack, _, _) <- getState

  case b of
    (LiteralValue p (B b), expr) -> return (LiteralValue p (B $ not b), op : expr)
    (Id p i, expr) -> return $ negValue (symTableGetVal (scopeNameVar act_name i) p state, op : expr)

unaArithExpr :: ParsecT [Token] State IO (Token, [Token])
unaArithExpr = do
  op <- minusToken
  n1 <- subExpression

  state@(_, _, (act_name, _) : stack, _, _) <- getState

  case n1 of
    (LiteralValue p (I i), expr) -> return (LiteralValue p (I (-i)), op : expr)
    (LiteralValue p (F f), expr) -> return (LiteralValue p (F (-f)), op : expr)
    (Id p i, expr) -> return $ negValue (symTableGetVal (scopeNameVar act_name i) p state, op : expr)

returnSt :: ParsecT [Token] State IO [Token]
returnSt = do
  ret <- returnToken

  (flag, symt, (act_name, depth) : _, _, subp) <- getState

  if not flag
    then do
      expr <- binExpr
      return $ ret : expr
    else do
      (v, expr) <- expression

      let (f_name, params, return_type, code) = getSubp act_name (pos ret) subp

      if isNothing return_type
        then do
          return $ ret : expr
        else do
          let expected_type = maybe "" typeof return_type
          let actual_type = typeof v

          when (actual_type /= expected_type) $
            error "Parameters with mismatched types."

          updateState $ returnSubp f_name (Just v)
          updateState $ setFlag False

          return $ ret : expr

-- function call as an expression
funCall :: ParsecT [Token] State IO (Token, [Token])
funCall = do
  f <- idToken
  lp <- beginpToken

  (flag, _, (act_name, depth) : _, _, subp) <- getState

  if not flag
    then do
      actual_parameters <- many exprCommaSt
      rp <- endpToken

      return (Eq (0, 0), f : lp : concat actual_parameters ++ [rp])
    else do
      actual_parameters <- many exprComma
      rp <- endpToken

      let scoped_name = functionName (name f)
      let (f_name, params, return_type, code) = getSubp scoped_name (pos f) subp

      if isNothing return_type
        then error "The function called has incompatible return type."
        else do
          updateState $ pushStack f_name

          if length params /= length actual_parameters
            then error "There are missing parameters!"
            else allocateParam actual_parameters params

          calle <- getInput
          setInput code
          block <- blockParser
          setInput calle

          (flag, _, (act_name, depth) : _, _, subp) <- getState
          let (f_name, params, return_type', code) = getSubp scoped_name (pos f) subp

          if return_type /= return_type'
            then do
              updateState $ returnSubp f_name return_type
              updateState $ setFlag True
              updateState $ symTableCleanScope f_name
              updateState popStack
              return (extractToken return_type', f : lp : concatMap snd actual_parameters ++ [rp])
            else do
              updateState $ symTableCleanScope f_name
              updateState popStack
              return (extractToken return_type', f : lp : concatMap snd actual_parameters ++ [rp])

allocateParam :: [(Token, [Token])] -> [(String, Token)] -> ParsecT [Token] State IO [Token]
allocateParam [] [] = return []
allocateParam ((actual_param, expr) : actual_params) ((formal_param, param_type) : formal_params) = do
  (_, _, (act_name, depth) : stack, _, _) <- getState

  let expected_type = typeof param_type
  let actual_type = typeof actual_param

  when (expected_type /= actual_type) $
    error "Parameters with mismatched types."

  updateState $ symTableInsert (scopeNameVar act_name formal_param) (depth, Mut (0, 0), param_type, actual_param)

  allocateParam actual_params formal_params
