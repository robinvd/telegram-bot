{-# LANGUAGE OverloadedStrings #-}

module Telegram.Help where

import Telegram.Ext
import qualified Data.Text as T (pack)

helpInfo :: Action
helpInfo (chatId, _:"/timetable":_) = return $ sendMessageRequest (T.pack $ show chatId) "Fetches and shows your timetable."
helpInfo (chatId, _:"/timetableRegister":_) = return $ sendMessageRequest (T.pack $ show chatId) "Sets your course and your groups. Ex: \"/timetableRegister Inf+B+1 CS 3,Pre-master Informatica Hanze en Noordelijke Hogeschool\""
helpInfo (chatId, _) = return $ sendMessageRequest (T.pack $ show chatId) msg
  where msg = "Simple bot made by Robin to show timetables from the rug. available commands: /help, /timetable, /timetableRegister. /help <command> for more info"
