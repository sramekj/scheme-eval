module Main where

import           Eval                           ( eval )
import           Parsers                        ( readExpr )
import           System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
