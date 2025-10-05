module Parser where

import Control.Applicative
import Data.Char (chr, isDigit)
import Data.List (nub)
import Data.Map (Map, fromList)
import Data.Maybe qualified

newtype Parser a = Parser
  { runParser :: String -> Either String (a, String)
  }

data Json = Obj (Map String Json) | Arr [Json] | Str String | Num Double | JTrue | JFalse | Null deriving (Show, Eq)

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Applicative Parser where
  pure a = Parser $ \input -> Right (a, input)

  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad Parser where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

instance Alternative Parser where
  empty = Parser $ \_ -> Left ""

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ err <> " and " <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)

instance (Semigroup a) => Semigroup (Parser a) where
  (<>) p1 p2 = Parser $ \input -> do
    (x, r) <- runParser p1 input
    (y, r') <- runParser p2 r
    pure (x <> y, r')

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input ->
  case input of
    [] -> Left "End of input"
    hd : rest
      | predicate hd -> Right (hd, rest)
      | otherwise -> Left $ "Unexpected " ++ [hd]

char :: Char -> Parser Char
char c = satisfy (== c)

strings :: String -> Parser String
strings = traverse char

ws :: Parser [Char]
ws = many $ satisfy (`elem` [' ', '\t', '\n', '\r'])

characterMap = [("\\n", "\n"), ("\\/", "/"), ("\\\"", "\""), ("\\f", "\f"), ("\\\\", "\\"), ("\\r", "\r"), ("\\t", "\t"), ("\\b", "\b")]

startsWith :: String -> String -> Bool
startsWith _ [] = True
startsWith (x : xs) (y : ys) = x == y && startsWith xs ys

findChar :: String -> String
findChar s = case lookup s characterMap of
  Nothing -> if startsWith s "\\u" then (: []) $ chr (read ("0x" ++ drop 2 s) :: Int) else s
  Just val -> val

character :: Parser [Char]
character = transform (satisfy (\x -> x /= '"' && x /= '\\' && x >= chr 0x0020 && x <= chr 0x10FFFF)) <|> findChar <$> transform (satisfy (== '\\')) <> escape

parseHex :: Parser [Char]
parseHex = hex <> hex <> hex <> hex

hex :: Parser [Char]
hex = transform (digit <|> satisfy (\x -> (x >= 'A' && x <= 'F') || (x >= 'a' && x <= 'f')))

escape :: Parser [Char]
escape = transform (satisfy (\x -> x `elem` ['"', '\\', '/', 'b', 'f', 'n', 'r', 't'])) <|> transform (char 'u') <> hex <> hex <> hex <> hex

characters :: Parser [Char]
characters = concat <$> many character

digit :: Parser Char
digit = satisfy isDigit

digits :: Parser [Char]
digits = some digit

onenine :: Parser Char
onenine = satisfy (\x -> x >= '1' && x <= '9')

transform :: Parser Char -> Parser [Char]
transform p = (: []) <$> p

integer :: Parser [Char]
integer = transform onenine <> digits <|> (: []) <$> digit <|> transform (satisfy (== '-')) <> transform onenine <> digits <|> transform (satisfy (== '-')) <> transform digit

fraction :: Parser [Char]
fraction = transform (satisfy (== '.')) <> digits <|> emptyParser

sign :: Parser [Char]
sign = transform (satisfy (== '+')) <|> transform (satisfy (== '-')) <|> emptyParser

emptyParser :: Parser [Char]
emptyParser = Parser $ \x -> Right ("", x)

parseExp :: Parser [Char]
parseExp = transform (satisfy (\x -> x == 'e' || x == 'E')) <> sign <> digits <|> emptyParser

parseNumber :: Parser Json
parseNumber = do
  i <- integer
  f <- fraction
  e <- parseExp
  return $ Num $ read $ i ++ f ++ e

parseString :: Parser Json
parseString = do
  satisfy (== '"')
  x <- characters
  satisfy (== '"')
  return $ Str x

parseTrue :: Parser Json
parseTrue = JTrue <$ strings "true"

parseFalse :: Parser Json
parseFalse = JFalse <$ strings "false"

parseNull :: Parser Json
parseNull = Null <$ strings "null"

parseValue :: Parser Json
parseValue = parseObject <|> parseArray <|> parseString <|> parseNumber <|> parseTrue <|> parseFalse <|> parseNull

parseElement :: Parser Json
parseElement = do
  ws
  val <- parseValue
  ws
  return val

parseElements :: Parser [Json]
parseElements = do
  x <- parseElement
  y <- many (char ',' *> parseElement)
  return $ x : y

emptyExpParser :: Parser [Json]
emptyExpParser = Parser $ \x -> Right ([], x)

parseArray :: Parser Json
parseArray = do
  char '['
  x <- parseElements <|> emptyExpParser
  char ']'
  return $ Arr x

parseMember :: Parser (String, Json)
parseMember = do
  ws
  x <- parseString
  ws
  char ':'
  y <- parseElement
  case x of
    Str s -> return (s, y)
    _ -> Parser $ \x -> Left "Wrong parsed"

parseMembers :: Parser [(String, Json)]
parseMembers = do
  x <- parseMember
  y <- many (char ',' *> parseMember)
  return $ x : y

parseObject :: Parser Json
parseObject = do
  char '{'
  members <- parseMembers <|> Parser (\x -> Right ([], x))
  char '}'
  return $ Obj $ fromList members

parseJson :: Parser Json
parseJson = parseElement

parse :: String -> Either String Json
parse s = do
  (j, r) <- runParser parseJson s
  if not $ null r then Left "Parsing Error" else return j