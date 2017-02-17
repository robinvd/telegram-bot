{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Default (def)
import           System.Environment (getArgs)
import           Telegram
import           Web.Telegram.API.Bot
import           Telegram.Help
import           Telegram.Timetable
import qualified Data.Text as T
import qualified Data.Map as M


main = do
  args <- getArgs
  case args of
    [] -> putStrLn "not token given, quiting!"
    _  -> telegram def {token = Token $ T.pack $ head args, actions = actions}
    where
      actions = M.fromList [ ("/t", timetable)
                           , ("/timetable", timetable)
                           , ("/tr", timetableRegister)
                           , ("/timetableRegister", timetableRegister)
                           , ("/h", helpInfo)
                           , ("/help", helpInfo)]
