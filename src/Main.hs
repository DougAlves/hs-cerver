{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket
import HttpParser
import System.Environment
import qualified Network.Socket.ByteString.Lazy as Bs
import Data.Monoid
import Data.String
import qualified Data.Map as M
import JsonParser
import qualified Data.ByteString.Lazy.Char8 as R
import System.IO as S
import System.Directory


data ServerState = St {
  port :: String,
  rootPath :: String
                      } 

httpHeader :: String
httpHeader = "HTTP/1.1 ok 200\n" <> "Content-Type: text/html\n"
 

manageRequest :: String -> HttpRequest
manageRequest xs = case httpRequest $ xs of
                     Right x -> x
                     Left x -> Req { method = Get, path = "Eror in :" <> x }

unsoc :: [a] -> Maybe [a]
unsoc [] = Nothing
unsoc [x] = Just []
unsoc (x:xs) = (x:) <$> unsoc xs

returnHtmlFile :: HttpRequest -> ServerState ->  IO String
returnHtmlFile (Req {method = _, path = file})
  (St {rootPath = x}) = do
  putStrLn x
  exists <- doesFileExist (x <> file)
  if exists
    then do input <- S.readFile $ x <> file
            return input
    else do input <- S.readFile $ x <> "index.html"
            return input 



mainLoop :: Socket -> ServerState -> IO ()
mainLoop sock st = do
    (conn, addr) <- accept sock
    req <- Bs.recv conn (5000)
    let coolReq = manageRequest $ filter (/='"') $ show req
    htmlReq <- returnHtmlFile coolReq st
    Bs.send conn (R.pack htmlReq)
    close conn
    mainLoop sock st

getFile :: [String] -> String
getFile [] = "magic.json"
getFile (y:_) = y


buildState :: Json -> ServerState
buildState (JsonObject x) =
  St {
  rootPath =( case M.lookup "root_path" x of
           Just (JsonString xs) -> xs
           Nothing -> "/var/.www" ),
  port = case M.lookup "port" x of
           Just (JsonString pt) -> pt
           Nothing -> "5050"
     }

main :: IO ()
main = do
  configfigF <- getFile <$> getArgs
  Right configJson <- parserFile configfigF
  let st = buildState configJson
  addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just $ port st)
  sock <- socket AF_INET Stream 0
  bind sock (addrAddress addr)
  listen sock 4
  mainLoop sock st

