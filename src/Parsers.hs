module Parsers
  ( readExpr
  ) where

import           Data.Functor                   ( (<&>) )
import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           Text.ParserCombinators.ReadPrec
                                                ( reset )

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

parseString :: Parser LispVal
parseString = do
  char '"'
  str <- many (noneOf "\"")
  char '"'
  return $ String str

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = [first] <> rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = many1 digit <&> (Number . read)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> "No match: " <> show err
  Right val -> "Found value"
