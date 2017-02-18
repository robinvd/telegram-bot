module Telegram.Ext (Action, sendMessageRequest, registerAction, registerCleanUp, Setup) where

import qualified Data.Map as M (Map)
import qualified Data.Text as T (Text)
import           Web.Telegram.API.Bot (SendMessageRequest, sendMessageRequest)
import           Control.Monad.Writer
import qualified Data.Map as M

type Action = ((Int, [T.Text]) -> IO SendMessageRequest)

type Setup = WriterT (M.Map T.Text Action) (WriterT [IO ()] IO ) ()

registerAction :: T.Text -> Action -> Setup
registerAction key action = tell (M.singleton key action)

registerCleanUp :: IO () -> Setup
registerCleanUp f = lift $ tell [f]
