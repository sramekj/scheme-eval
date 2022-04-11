{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Eval
  ( eval
  , IOThrowsError(..)
  , liftThrows
  , primitiveBindings
  ) where

import           Control.Exception              ( evaluate )
import           Control.Monad.Except
import           Data.Functor                   ( (<&>) )
import           Data.IORef
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )
import           GHC.ExecutionStack             ( Location(functionName) )
import           GHC.GHCi.Helpers               ( evalWrapper )
import           GHC.RTS.Flags                  ( ParFlags(ParFlags) )
import           Parsers

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err
liftThrows (Right val) = return val

makeFunc
  :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body =
  return $ Func (map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarargs = makeFunc . Just . showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _                             ) = return val
eval env val@(Number _                             ) = return val
eval env val@(Bool   _                             ) = return val
eval env (    Atom   id                            ) = getVar env id
eval env (    List   [Atom "quote", val]           ) = return val
eval env (    List   [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    _          -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body))
  = makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarargs varargs env [] body
eval env (List (function : args)) = do
  func    <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func {..}         ) args = if num params /= num args && isNothing vararg
  then throwError $ NumArgs (num params) args
  else
    liftIO (bindVars closure $ zip params args)
    >>= bindVarArgs vararg
    >>= evalBody
 where
  remainingArgs = drop (length params) args
  num           = toInteger . length
  evalBody env = last <$> mapM (eval env) body
  bindVarArgs arg env = case arg of
    Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
    Nothing      -> return env

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+"        , numBinOp (+))
  , ("-"        , numBinOp (-))
  , ("*"        , numBinOp (*))
  , ("/"        , numBinOp div)
  , ("mod"      , numBinOp mod)
  , ("quotient" , numBinOp quot)
  , ("remainder", numBinOp rem)
  , ("="        , numBoolBinOp (==))
  , ("<"        , numBoolBinOp (<))
  , (">"        , numBoolBinOp (>))
  , ("/="       , numBoolBinOp (/=))
  , (">="       , numBoolBinOp (>=))
  , ("<="       , numBoolBinOp (<=))
  , ("&&"       , boolBoolBinOp (&&))
  , ("||"       , boolBoolBinOp (||))
  , ("string=?" , strBoolBinOp (==))
  , ("string?"  , strBoolBinOp (>))
  , ("string<=?", strBoolBinOp (<=))
  , ("string>=?", strBoolBinOp (>=))
  , ("car"      , car)
  , ("cdr"      , cdr)
  , ("cons"     , cons)
  , ("eq?"      , eqv)
  , ("eqv?"     , eqv)
  , ("equal?"   , equal)
  ]

primitiveBindings :: IO Env
primitiveBindings = nullEnv
  >>= flip bindVars (map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
  in  if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool   s) = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

numBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numBinOp op params        = mapM unpackNum params <&> Number . foldl1 op

boolBinOp
  :: (LispVal -> ThrowsError a)
  -> (a -> a -> Bool)
  -> [LispVal]
  -> ThrowsError LispVal
boolBinOp unpacker op args = if length args /= 2
  then throwError $ NumArgs 2 args
  else do
    left  <- unpacker $ head args
    right <- unpacker $ args !! 1
    return $ Bool $ left `op` right

numBoolBinOp = boolBinOp unpackNum

strBoolBinOp = boolBinOp unpackStr

boolBoolBinOp = boolBinOp unpackBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)        ] = return x
car [DottedList (x : xs) _] = return x
car [badArg               ] = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)        ] = return $ List xs
cdr [DottedList [xs    ] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg               ] = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []            ] = return $ List [x1]
cons [x , List xs            ] = return $ List $ [x] <> xs
cons [x , DottedList xs xlast] = return $ DottedList ([x] <> xs) xlast
cons [x1, x2                 ] = return $ DottedList [x1] x2
cons badArgList                = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool   arg1, Bool arg2  ] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom   arg1, Atom arg2  ] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] =
  eqv [List $ xs <> [x], List $ ys <> [y]]
eqv [List arg1, List arg2] = return $ Bool $ length arg1 == length arg2 && and
  (zipWith (curry eqvPair) arg1 arg2)
 where
  eqvPair (x1, x2) = case eqv [x1, x2] of
    Right (Bool val) -> val
    _                -> False
eqv [_, _]     = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM
    (unpackEquals arg1 arg2)
    [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> (isJust . lookup var)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . flip writeIORef value)
        (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env      <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
 where
  extendEnv bindings env = fmap (<> env) (mapM addBinding bindings)
  addBinding (var, value) = do
    ref <- newIORef value
    return (var, ref)
