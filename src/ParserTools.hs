module ParserTools where

import Data.Char
import Control.Applicative
import Data.Monoid

instance (Monoid a, Eq a) => Alternative (Either a) where
  empty = Left mempty

  Left x <|> Right y = Right y
  Right x <|> _ = Right x
  Left x <|> Left y = if x == mempty then Left y else Left x

newtype Parser a = Parser
  {runParser :: String -> Either String (String, a)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> (f <$>) <$> p input

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)

  Parser pf <*> Parser pa = Parser $ \input -> do
    (input', f) <- pf input
    (input'', x) <- pa input'
    pure (input'', f x)

instance Alternative Parser where
  empty = Parser $ \input -> empty

  Parser p1 <|> Parser p2 = Parser $ \input ->
    case p1 input of
      Right x -> Right x
      Left y ->  case p2 input of
                   Right x -> Right x
                   Left y2 -> if y == empty then Left y2 else Left y
      

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:)  <$> element <*> many (sep *> element) <|> pure []

charP :: Char -> Parser Char
charP x = Parser f
  where f (y:ys)
          | y == x = Right (ys, x)
          | otherwise = Left $ "could not math expected char " <> [x] <> " with given char " <> [y]
        f [] = Left ""

stringP :: String -> Parser String
stringP = sequenceA . fmap charP 

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
  (inp, rest) <- p input
  if null rest
    then Left $ "Cound not parse " <> input
    else Right (inp, rest) 
  
spanP :: (Char -> Bool) -> Parser String
spanP p = Parser $ \input ->
  let (token,rest) = span p input
  in Right (rest, token) 

ws = spanP isSpace
