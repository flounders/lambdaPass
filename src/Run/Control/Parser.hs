module Run.Control.Parser
       ( fileParse
       , parseRunControl
       ) where

import Text.Parsec (parse, ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, noneOf, char, satisfy)
import Text.Parsec.Combinator (eof, many1)
import Data.Char (isLetter, isDigit)
import Control.Monad (void)
import Control.Applicative (many)

type Key   = String
type Value = String

type KeyValuePair = (Key, Value)

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

whitespace :: Parser ()
whitespace = void . many $ oneOf " \t"

parseKey :: Parser Key
parseKey = do
  x <- firstChar
  xs <- lexeme $ many nonFirstChar
  return (x:xs)
    where firstChar = satisfy (\c -> isLetter c || c == '_')
          nonFirstChar = satisfy (\c -> isLetter c || isDigit c || c == '_')

parseValue :: Parser Value
parseValue = do
  value <- lexeme $ many1 (noneOf " \n\t")
  return value

parseKeyValuePair :: Parser KeyValuePair
parseKeyValuePair = do
  key <- parseKey
  void $ lexeme $ char '='
  value <- lexeme $ parseValue
  void . many $ oneOf "\n\r"
  return (key, value)

parseRunControl :: Parser [KeyValuePair]
parseRunControl = do
  pairs <- many parseKeyValuePair
  eof
  return pairs

fileParse :: Parser a -> String -> IO (Either ParseError a)
fileParse p fn = do
  fContents <- readFile fn
  let result = parse p fn fContents
  return result
