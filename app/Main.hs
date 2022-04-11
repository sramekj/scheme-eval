module Main where

import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( liftM )
import           Control.Monad.Except
import           Data.Functor                   ( (<&>) )
import           Eval                           ( IOThrowsError
                                                , eval
                                                , liftThrows
                                                , primitiveBindings
                                                )
import           GHC.Event.Windows              ( processRemoteCompletion )
import           Parsers                        ( Env
                                                , ThrowsError
                                                , nullEnv
                                                , readExpr
                                                )
import           System.Environment
import           System.IO               hiding ( try )

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

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

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
  case length args of
    0 -> runRepl
    1 -> runOne $ head args
    _ -> putStrLn "Program takes only 0 or 1 argument"
