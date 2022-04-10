module Eval
  ( eval
  ) where

import           Control.Monad.Except
import           Data.Functor                   ( (<&>) )
import           Parsers

eval :: LispVal -> ThrowsError LispVal
eval val@(String _                  ) = return val
eval val@(Number _                  ) = return val
eval val@(Bool   _                  ) = return val
eval (    List   [Atom "quote", val]) = return val
eval (    List   (Atom f : args)    ) = mapM eval args >>= apply f
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
