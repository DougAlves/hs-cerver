{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket
import HttpParser
import System.Environment
import qualified Network.Socket.ByteString.Lazy as Bs
import Data.Monoid
import Data.String
import JsonParser

manageRequest :: String -> HttpRequest
manageRequest xs = case httpRequest $ xs of
                     Right x -> x
                     Left x -> Req { method = Get, path = "Eror in :" <> x }

unsoc :: [a] -> Maybe [a]
unsoc [] = Nothing
unsoc [x] = Just []
unsoc (x:xs) = (x:) <$> unsoc xs

mainLoop :: Socket -> IO ()
mainLoop sock = do
    (conn, addr) <- accept sock
    req <- Bs.recv conn (5000)
    putStrLn $ "Request não tratado : ===============================\n" <> show req
    let coolReq = manageRequest $ filter (/='"') $ show req
    putStrLn $ "Request Trado : ===============================\n" <> show coolReq
    Bs.send conn (response coolReq)
    close conn
    mainLoop sock

getPort :: [String] -> String
getPort [] = "5050"
getPort (p:_) = p

main :: IO ()
main = do
  port <- getPort <$> getArgs
  addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just port)
  sock <- socket AF_INET Stream 0
  bind sock (addrAddress addr)
  listen sock 4
  mainLoop sock

response :: (Monoid m, Data.String.IsString m) => HttpRequest -> m
response req
  | (path req == "/") ||  path req == "/favicon.ico" = "HTTP/1.1 200 ok\n" <>
                                                       "content-type: HTML\n\n" <>
                                                       "<html>" <>
                                                       "<head>" <>
                                                       "<title> Pure Functional Cerver </title>" <>
                                                       "</head>" <>
                                                       "<body>" <>
                                                       "<h1> Haskell Cerver </h1>" <>
                                                       "</body>" <>
                                                       "</html>\n"
  | otherwise =  "HTTP/1.1 200 ok\n" <>
                 "content-type: HTML\n\n" <>
                 "<html>" <>
                 "<head>" <>
                 "<title> Khelow  </title>" <>
                 "</head>" <>
                 "<body>" <>
                 "<h1> be welcome to the Haskell Cerver </h1>" <>
                 "</body>" <>
                 "</html>\n"
