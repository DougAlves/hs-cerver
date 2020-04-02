{-# LANGUAGE OverloadedStrings #-}
module HttpParser
  (httpRequest
  , HttpRequest (..)
  , Method (..)
  ) where

import Control.Applicative
import Data.Char
import Data.Foldable
import Data.Monoid
import qualified ParserTools as PT
import Control.Applicative
import Data.Monoid

data HttpRequest = Req
  {
    method :: Method,
    path :: String
  }
  deriving (Show, Eq)


data Method = Get | Post
  deriving (Show, Eq)

newtype Parser a = Parser
  {
    runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f p = Parser $ \input -> do
    (input', x) <- runParser p input
    pure (input', f x)


instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)

  Parser p1 <*> Parser p2 = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', x) <- p2 input'
    pure (input'', f x)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  Parser p1 <|> Parser p2 = Parser $ \input ->
    p1 input <|> p2 input


charP :: Char -> Parser Char
charP x = Parser f
  where f (y:ys)
          | y == x = Just (ys, x)
          | otherwise = Nothing
        f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP p = Parser $ \input ->
  let (token,rest) = span p input
  in Just (rest, token)


pathChar :: Char -> Bool
pathChar = getAny . predicate 
  where predicate = foldMap (Any .) [isAlphaNum, (== '/'),( == '.')]

ws = spanP isSpace

pathP :: Parser String
pathP = ws *> (spanP pathChar) <* ws <* stringP "HTTP" <* ws 

methodP :: Parser Method 
methodP = (const Get <$> stringP "GET") <|> (const Post <$> stringP "POST")

httpRequest :: String -> Maybe HttpRequest
httpRequest input = do
  (inp, met) <- runParser methodP input
  (_, pth) <- runParser pathP inp
  Just  $ Req {method = met, path = pth }
