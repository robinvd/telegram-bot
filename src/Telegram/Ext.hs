module Telegram.Ext (Action, sendMessageRequest) where

import qualified Data.Map as M (Map)
import qualified Data.Text as T (Text)
import           Web.Telegram.API.Bot (SendMessageRequest, sendMessageRequest)

type Action = ((Int, [T.Text]) -> IO SendMessageRequest)
