module Eval
  ( eval
  ) where

import           Control.Monad.Except
import           Data.Functor                   ( (<&>) )
import           Parsers

eval :: LispVal -> ThrowsError LispVal
eval val@(String _                             ) = return val
eval val@(Number _                             ) = return val
eval val@(Bool   _                             ) = return val
eval (    List   [Atom "quote", val]           ) = return val
eval (    List   [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    _          -> eval conseq
eval (List (Atom f : args)) = mapM eval args >>= apply f
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args =
  maybe (throwError $ NotFunction "Unrecognized primitive function args" f)
        ($ args)
    $ lookup f primitives

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
  ]

numBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numBinOp op params        = mapM unpackNum params <&> (Number . foldl1 op)

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
