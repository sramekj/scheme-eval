module Main where

import           Parsers                        ( readExpr )
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Hello, " ++ readExpr (head args)
