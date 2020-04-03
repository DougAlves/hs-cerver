module JsonParser where

import Data.Map
import Data.Char
import Control.Applicative
import ParserTools
import Data.List

data Json = JsonNull
          | JsonNumber Integer
          | JsonString String
          | JsonBool Bool
          | JsonArray [Json]
          | JsonObject (Map String Json)
          deriving(Show, Eq)

jsonNull :: Parser Json
jsonNull = const JsonNull <$> stringP "null" 

jsonNumber :: Parser Json
jsonNumber = f <$> notNull (spanP isDigit)
  where f ds = JsonNumber $ read ds
                          
jsonBool :: Parser Json
jsonBool = (\_ -> JsonBool True) <$> stringP "true" <|> (\_ -> JsonBool False) <$> stringP "false" 

jsonString :: Parser Json
jsonString = JsonString <$> stringLiteral 

stringLiteral :: Parser String
stringLiteral = (charP '"' *> spanP (/='"') <* charP '"')

jsonArray :: Parser Json
jsonArray = JsonArray <$> ( charP '[' *> ws *> elements <* ws <* charP ']')
  where elements = sepBy sep jsonValue
        sep  = ws *> charP ',' <* ws

jsonObject :: Parser Json
jsonObject = f <$> (charP '{' *> ws *> sepBy (ws *>charP ',' <* ws) pair <* ws <*charP '}')
  where pair = (\key _ val -> (key, val)) <$> stringLiteral <*> (ws *> charP ':' <* ws ) <*> jsonValue
        f    = (JsonObject . fromAscList . sortBy g )
        g (str1, _) (str2,_)
          | str1 == str2 = EQ
          | str1 <= str2 = LT
          | otherwise = GT

jsonValue :: Parser Json
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

parserFile :: FilePath -> IO (Either String Json)
parserFile fileName = do
  input <- readFile fileName
  return (snd <$> runParser jsonValue input)

  
