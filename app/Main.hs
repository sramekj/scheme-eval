module Main where

import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( liftM )
import           Control.Monad.Except
import           Data.Functor                   ( (<&>) )
import           Eval                           ( bindVars
                                                , eval
                                                , liftThrows
                                                , primitiveBindings
                                                )
import           Parsers                        ( readExpr )
import           System.Environment
import           System.IO               hiding ( try )
import           Types

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result then return () else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args =
  do
    env <- primitiveBindings
      >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows $ show <$> eval env (List [Atom "load", String (head args)])
  >>= hPutStrLn stderr

runRepl :: IO ()
runRepl =
  primitiveBindings
    >>= until_ (liftA2 (||) (== "quit") (== ":q")) (readPrompt "Lisp>>> ")
    .   evalAndPrint

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _           = error "Invalid operation"

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne args
