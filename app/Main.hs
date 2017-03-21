{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Environment (getArgs)
import           Web.Telegram.API.Bot
import           Telegram
import           Telegram.Ext
import           Telegram.Help
import           Telegram.Timetable
import qualified Data.Text as T (pack)



main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "no token given, quiting!"
    _  -> telegram (settings args) actions
  where
    settings x = TelegramSettings
      { token = Token . T.pack . head $ x
      , updateDelay = 1000000
      , defaultMsg = errorMessage}
    actions = do
      helpInfo
      timetable



-- | the default error message if no command is found
-- | (TODO) should not be used in multi bot chat groups
errorMessage :: Action
errorMessage (chatId, input) = return $ sendMessageRequest chatId (head input)
