module Parsers
  ( readExpr
  , readExprList
  ) where

import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Functor                   ( (<&>) )
import           Numeric                        ( readFloat
                                                , readHex
                                                , readOct
                                                )
import           Text.ParserCombinators.Parsec  ( Parser
                                                , alphaNum
                                                , anyChar
                                                , char
                                                , digit
                                                , endBy
                                                , hexDigit
                                                , letter
                                                , many
                                                , many1
                                                , noneOf
                                                , notFollowedBy
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

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space") <|> do
    x <- anyChar
    notFollowedBy alphaNum
    return [x]
  return $ Character $ case value of
    "space"   -> ' '
    "newline" -> '\n'
    _         -> head value

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float (fst . head $ readFloat (x <> "." <> y))

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
  parseAtom
    <|> parseString
    <|> try parseFloat
    <|> try parseNumber
    <|> try parseBool
    <|> try parseCharacter
    <|> parseQuoted
    <|> do
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
