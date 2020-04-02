module JsonParser where

import Data.Map

data JsonValue = JsonNull
               | JsonNumber Integer
               | JsonString String
               | JsonBool Bool
               | JsonArray [JsonValue]
               | JsonObjec (Map String JsonValue)

