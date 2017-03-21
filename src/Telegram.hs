{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram (telegram, TelegramSettings(..)) where


import           Control.Concurrent
import           Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe (mapMaybe)
import qualified Data.Map as M
import           Network.HTTP.Client      (newManager, Manager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           System.Posix.Signals
import           Web.Telegram.API.Bot

import Control.Monad.Writer

import Telegram.Ext

-- | available settings with the defaults
data TelegramSettings = TelegramSettings {
  token :: Token,
  updateDelay :: Int,
  defaultMsg :: Action}

-- | Chains a monadic function together feeding the output into the input
-- | Delays after every iteration
chainMDelay :: (a -> IO a) -> a -> Int -> IO a
chainMDelay f a delay = do
  x <- f a
  threadDelay delay
  chainMDelay f x delay


-- | function for handling a SIGTERM
termHandler :: MVar () -> Handler
termHandler quitting = CatchOnce $ do
  putStrLn "termhandler quitting"
  putMVar quitting ()


-- | main function
-- | initializes the settings and checks if token is valid
-- | if the token is valid the main loop is started
telegram :: TelegramSettings -> Setup -> IO()
telegram TelegramSettings{..} setup = do
  quitting <- newEmptyMVar
  installHandler sigTERM (termHandler quitting) Nothing
  installHandler sigINT (termHandler quitting) Nothing
  manager <- newManager tlsManagerSettings
  botInfo <- getMe token manager
  (actions, cleanups) <- runWriterT . execWriterT $ setup
  case botInfo of
    Left e-> do
      T.putStrLn "server start failed on checking bot account! Maybe your token is not valid?"
      print e
    Right Response { result = u } -> do
      putStr "starting loop with account: "
      print $ user_first_name u
      fid <- forkIO $ void $ chainMDelay (update manager token actions) 0 3000000
      takeMVar quitting
      killThread fid
      putStrLn "signal recieved, quiting! running cleanup functions"
      sequence_ cleanups
      return ()

-- | Issues a single update to the telegram server and finds the function to run
-- | (TODO) The function in the where clause are a bit messy
-- | (TODO) use the defaultmsg from the settings instead of a hardcoded one!
update :: Manager -> Token -> M.Map T.Text Action -> Int -> IO Int
update manager token actions updateId = do
  telegramUpdate <- getUpdates token (Just updateId) Nothing Nothing manager
  putStrLn $ "updating " ++ show updateId
  print telegramUpdate
  case telegramUpdate of
    Left e-> do
      putStrLn "Request failed, but not quiting!"
      print e
      return updateId
    Right Response {result = updates} -> do
      mapM_ ((forkIO . void . send) <=< getAction) formatedMsgs
      return $ maximum $ updateId:map ((1+) . update_id) updates
      where
        getAction :: (ChatId, [T.Text]) -> IO SendMessageRequest
        getAction input@(_, x) = case M.lookup (head x) actions of
          Just act -> act input
          Nothing -> errorMessage input
        msgs :: [Message]
        msgs = mapMaybe message updates
        formatedMsgs :: [(ChatId, [T.Text])]
        formatedMsgs = mapMaybe f msgs
        f :: Message -> Maybe (ChatId, [T.Text])
        f Message {text = Nothing} = Nothing
        f Message {text = Just t, chat = ch} = Just (ChatId . toInteger . chat_id $ ch, T.words t)
        send request = sendMessage token request manager

errorMessage :: Action
errorMessage (chatId, input) = return $ sendMessageRequest chatId (head input)
