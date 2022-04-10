module Parsers
  ( readExpr
  , LispVal(..)
  ) where

import           Data.Functor                   ( (<&>) )
import           Text.ParserCombinators.Parsec  ( Parser
                                                , char
                                                , digit
                                                , endBy
                                                , letter
                                                , many
                                                , many1
                                                , noneOf
                                                , oneOf
                                                , parse
                                                , sepBy
                                                , skipMany1
                                                , space
                                                , try
                                                , (<|>)
                                                )

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

parseList :: Parser LispVal
parseList = sepBy parseExpr spaces <&> List

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  ex <- parseExpr
  return $ List [Atom "quote", ex]

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> do
  char '('
  exp <- try parseList <|> parseDottedList
  char ')'
  return exp

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> String $ "No match: " <> show err
  Right val -> val

showVal :: LispVal -> String
showVal (String contents) = "\"" <> contents <> "\""
showVal (Atom   name    ) = name
showVal (Number contents) = show contents
showVal (Bool   True    ) = "#t"
showVal (Bool   False   ) = "#f"
showVal (List   contents) = "(" <> unwordsList contents <> ")"
showVal (DottedList head tail) =
  "(" <> unwordsList head <> " . " <> showVal tail <> ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal
