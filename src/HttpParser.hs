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
    path :: String
  }
  deriving (Show, Eq)


data Method = Get | Post
  deriving (Show, Eq)

pathChar :: Char -> Bool
pathChar = getAny . predicate 
  where predicate = foldMap (Any .) [isAlphaNum, (== '/'),( == '.')]

pathP :: Parser String
pathP = ws *> (spanP pathChar) <* ws <* stringP "HTTP"  

methodP :: Parser Method 
methodP = (const Get <$> stringP "GET") <|> (const Post <$> stringP "POST") 

httpRequest :: String -> Either String HttpRequest
httpRequest input = do
  (inp, met) <- runParser methodP input
  (_, pth) <- runParser pathP inp
  pure $ Req {method = met, path = pth }
