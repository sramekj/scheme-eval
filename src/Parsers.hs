module Parsers
  ( readExpr
  , readExprList
  ) where

import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Functor                   ( (<&>) )
import           Numeric                        ( readHex
                                                , readOct
                                                )
import           Text.ParserCombinators.Parsec  ( Parser
                                                , char
                                                , digit
                                                , endBy
                                                , hexDigit
                                                , letter
                                                , many
                                                , many1
                                                , noneOf
                                                , octDigit
                                                , oneOf
                                                , parse
                                                , sepBy
                                                , skipMany1
                                                , space
                                                , string
                                                , try
                                                , (<|>)
                                                )
import           Types

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  c <- oneOf "\\\"nrt"
  return $ case c of
    '\\' -> c
    '"'  -> c
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'

parseString :: Parser LispVal
parseString = do
  char '"'
  str <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String str

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = [first] <> rest
  return $ Atom atom

parseNumber :: Parser LispVal
parseNumber =
  parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit <&> Number . read

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
  try $ string "#d"
  str <- many1 digit
  (return . Number . read) str

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  str <- many1 hexDigit
  return $ Number (hex2dig str)

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  str <- many1 octDigit
  return $ Number (oct2dig str)

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  str <- many1 (oneOf "10")
  return $ Number (bin2dig str)

oct2dig x = fst $ head (readOct x)

hex2dig x = fst $ head (readHex x)

bin2dig = bin2dig' 0

bin2dig' digint "" = digint
bin2dig' digint (x : xs) =
  let old = 2 * digint + (if x == '0' then 0 else 1) in bin2dig' old xs

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

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
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseBool <|> parseQuoted <|> do
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
