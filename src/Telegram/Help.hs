{-# LANGUAGE OverloadedStrings #-}

module Telegram.Help where

import qualified Data.Text as T (pack)
import Telegram.Ext

helpInfo :: Setup
helpInfo = mapM_ (registerAction action) ["/h", "/help"]

action :: Action
action (chatId, _:"timetable":_) = return $ sendMessageRequest chatId
  "Fetches and shows your timetable."
action (chatId, _:"timetableRegister":_) = return $ sendMessageRequest chatId
  "Add a course: \"/timetableRegister <CourseCode> <groups>\" . Ex: \"/timetableRegister Inf+B+1 CS 3,Pre-master Informatica Hanze en Noordelijke Hogeschool\""
action (chatId, _:"reset":_) = return $ sendMessageRequest chatId
  "clear registered courses, after this you have to register again with /timetableRegister"
action (chatId, _) = return $ sendMessageRequest chatId
  "Bot made by Robin to show timetables from the rug.\n\navailable commands:\n/help, /timetable, /timetableRegister /reset.\n/help <command> for more info\n\nSee https://github.com/robinvd/telegram-bot for more info"
