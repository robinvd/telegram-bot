{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram (telegram, TelegramSettings(..)) where


import           Network.HTTP.Client      (newManager, Manager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import           Data.Default
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe (mapMaybe)
import qualified Data.Map as M
import           Control.Concurrent (threadDelay)

import Telegram.Ext

data TelegramSettings = TelegramSettings {
  token :: Token,
  updateDelay :: Int,
  actions :: M.Map T.Text Action,
  defaultMsg :: Action}
instance Default TelegramSettings where
  def = TelegramSettings (Token "") 1000000 M.empty errorMessage

chainMDelay :: (a -> IO a) -> a -> Int -> IO a
chainMDelay f a delay = do
  x <- f a
  threadDelay delay
  chainMDelay f x delay

telegram :: TelegramSettings -> IO()
telegram TelegramSettings{..} = do
  manager <- newManager tlsManagerSettings
  botInfo <- getMe token manager
  case botInfo of
    Left error -> do
      T.putStrLn "server start failed on checking bot account! Maybe your token is not valid?"
      T.appendFile "updates.log" (T.pack . show $ error)
    Right Response { result = u } -> do
      putStr "starting loop with account: "
      print $ user_first_name u
      chainMDelay (update manager token actions) 0 3000000
      return ()

update :: Manager -> Token -> M.Map T.Text Action -> Int -> IO Int
update manager token actions updateId = do
  putStrLn $ "updating " ++ show updateId
  update <- getUpdates token (Just updateId) Nothing Nothing manager
  appendFile "updates.log" $ show update ++ "\n"
  case update of
    Left e -> do
      putStrLn "Request failed, see logs for more details!"
      T.appendFile "updates.log" (T.pack . show $ e)
      return updateId
    Right Response {result = updates} -> do
      mapM_ (\x -> getAction x >>= send) formatedMsgs
      return $ maximum $ updateId:map ((1+) . update_id) updates
      where
        getAction :: (Int, [T.Text]) -> IO SendMessageRequest
        getAction input@(chatId, x) = case M.lookup (head x) actions of
          Just act -> act input
          Nothing -> errorMessage input
        msgs :: [Message]
        msgs = mapMaybe message updates
        formatedMsgs :: [(Int, [T.Text])]
        formatedMsgs = mapMaybe f msgs
        f :: Message -> Maybe (Int, [T.Text])
        f Message {text = Nothing} = Nothing
        f Message {text = Just t, chat = ch} = Just (chat_id ch, T.lines t)
        send request = sendMessage token request manager

errorMessage :: Action
errorMessage (chatId, input) = return $ sendMessageRequest (T.pack $ show chatId) ("Message not understood :")
