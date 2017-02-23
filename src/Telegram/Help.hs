{-# LANGUAGE OverloadedStrings #-}

module Telegram.Help where

import qualified Data.Text as T (pack)
import Telegram.Ext

helpInfo :: Setup
helpInfo = registerAction "/h" action

action :: Action
action (chatId, _:"timetable":_) = return $ sendMessageRequest (T.pack $ show chatId)
  "Fetches and shows your timetable."
action (chatId, _:"timetableRegister":_) = return $ sendMessageRequest (T.pack $ show chatId)
  "Sets your course and your groups. Ex: \"/timetableRegister Inf+B+1 CS 3,Pre-master Informatica Hanze en Noordelijke Hogeschool\""
action (chatId, _) = return $ sendMessageRequest (T.pack $ show chatId)
  "Simple bot made by Robin to show timetables from the rug. available commands: /help, /timetable, /timetableRegister. /help <command> for more info"
