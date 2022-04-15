{-# LANGUAGE NamedFieldPuns #-}

module Types
  ( IOThrowsError(..)
  , Env(..)
  , LispVal(..)
  , LispError(..)
  , ThrowsError(..)
  , nullEnv
  , showVal
  ) where

import           Control.Monad.Except
import           Data.IORef
import           System.IO                      ( Handle )
import           Text.ParserCombinators.Parsec.Error
                                                ( ParseError )

type IOThrowsError = ExceptT LispError IO

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Character Char
  | Float Double
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func {params :: [String], vararg :: Maybe String, body :: [LispVal], closure :: Env}
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

type ThrowsError = Either LispError

showVal :: LispVal -> String
showVal (String contents) = "\"" <> contents <> "\""
showVal (Atom   name    ) = name
showVal (Number contents) = show contents
showVal (Bool   True    ) = "#t"
showVal (Bool   False   ) = "#f"
showVal (List   contents) = "(" <> unwordsList contents <> ")"
showVal (DottedList head tail) =
  "(" <> unwordsList head <> " . " <> showVal tail <> ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func { params, vararg, body, closure }) =
  "(lambda ("
    <> unwords (map show params)
    <> (case vararg of
         Nothing  -> ""
         Just arg -> " . " <> arg
       )
    <> ") ...)"
showVal (Port   _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal

showError :: LispError -> String
showError (UnboundVar     message varname) = message <> ": " <> varname
showError (BadSpecialForm message form   ) = message <> ": " <> show form
showError (NotFunction    message func   ) = message <> ": " <> show func
showError (NumArgs expected found) =
  "Expected " <> show expected <> " args: found values " <> unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " <> expected <> ", found " <> show found
showError (Parser  parseErr) = "Parse error at " <> show parseErr
showError (Default message ) = message

instance Show LispError where
  show = showError
