module Telegram.Ext (Action, sendMessageRequest, registerAction, registerCleanUp, Setup) where

import qualified Data.Map as M (Map, singleton)
import qualified Data.Text as T (Text)
import           Web.Telegram.API.Bot (SendMessageRequest, sendMessageRequest, ChatId)
import           Control.Monad.Writer

type Action = ((ChatId, [T.Text]) -> IO SendMessageRequest)

type Setup = WriterT (M.Map T.Text Action) (WriterT [IO ()] IO ) ()

registerAction :: Action -> T.Text -> Setup
registerAction action key = tell (M.singleton key action)

registerCleanUp :: IO () -> Setup
registerCleanUp f = lift $ tell [f]
