{-# LANGUAGE OverloadedStrings #-}
module HttpParser
  (httpRequest
  , HttpRequest (..)
  , Method (..)
  , methodP
  ) where

import Control.Applicative
import Data.Char
import Data.Foldable
import Data.Monoid
import qualified ParserTools as PT
import Control.Applicative
import Data.Monoid
import ParserTools

data HttpRequest = Req
  {
    method :: Method,
    path :: String,
    host :: String
  }
  deriving (Show, Eq)


data Method = Get | Post
  deriving (Show, Eq)

type Path = String

pathChar :: Char -> Bool
pathChar = getAny . predicate 
  where predicate = foldMap (Any .) [isAlphaNum, (== '/'),( == '.')]

pathP :: Parser Path
pathP = ws *> (spanP pathChar) <* ws <* (stringP $ "HTTP/1.1\\r\\n")

methodP :: Parser Method 
methodP = (const Get <$> stringP "GET") <|> (const Post <$> stringP "POST") 

hostP :: Parser String
hostP = ws *> methodP *> ws *> pathP *> ws *> (stringP "Host:") *> ws *> (spanP (/='\\')) 

httpRequest :: String -> Either String HttpRequest
httpRequest input = do
  (inp, met) <- runParser methodP input
  (_, pth) <- runParser pathP inp
  host <- snd <$> runParser hostP input
  pure $ Req {method = met, path = pth, host = host }
