{-# LANGUAGE OverloadedStrings #-}

module Telegram.Help where

import Telegram.Ext
import qualified Data.Text as T (pack)

helpInfo :: Action
helpInfo (chatId, _) = return $ sendMessageRequest (T.pack $ show chatId) msg
  where msg = "Simple bot made by Robin to show timetables from the rug. available commands: /help, /timetable"
