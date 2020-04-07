{-# LANGUAGE OverloadedStrings #-}

module Log where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Semigroup
import Data.Time.Clock

logInfo :: T.Text -> IO () 
logInfo xs = do
  curTime <- show <$> getCurrentTime
  T.putStrLn ("[INFO " <> (T.pack curTime) <> " ] "<> xs)

logErr :: T.Text -> IO () 
logErr xs =  do
  curTime <- show <$> getCurrentTime
  T.putStrLn ("[ERROR " <> (T.pack curTime) <> " ] "<> xs)


logWarnig :: T.Text -> IO () 
logWarnig xs = do
  curTime <- show <$> getCurrentTime
  T.putStrLn ("[WARNIG " <> (T.pack curTime) <> " ] "<> xs)

