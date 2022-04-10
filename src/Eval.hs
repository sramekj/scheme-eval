module Eval
  ( eval
  ) where

import           Parsers

eval :: LispVal -> LispVal
eval val@(String _                  ) = val
eval val@(Number _                  ) = val
eval val@(Bool   _                  ) = val
eval (    List   [Atom "quote", val]) = val
eval (    List   (Atom f : args)    ) = apply f $ map eval args

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ lookup f primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+"        , numBinOp (+))
  , ("-"        , numBinOp (-))
  , ("*"        , numBinOp (*))
  , ("/"        , numBinOp div)
  , ("mod"      , numBinOp mod)
  , ("quotient" , numBinOp quot)
  , ("remainder", numBinOp rem)
  ]

numBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numBinOp op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) =
  let parsed = reads n in if null parsed then 0 else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _          = 0
