module Main where

import           Control.Monad                  ( liftM )
import           Eval                           ( eval )
import           Parsers                        ( extractValue
                                                , readExpr
                                                , trapError
                                                )
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
