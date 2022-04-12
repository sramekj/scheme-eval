module Parsers
  ( readExpr
  , readExprList
  ) where

import           Control.Monad.Except           ( MonadError
                                                  ( catchError
                                                  , throwError
                                                  )
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.IORef
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
import           Types

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

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left  err -> throwError $ Parser err
  Right val -> return val

readExpr = readOrThrow parseExpr

readExprList = readOrThrow (endBy parseExpr spaces)
