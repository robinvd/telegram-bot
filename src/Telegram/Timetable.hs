{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Timetable (timetable) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as BL8 (pack)
import           Data.Time (getCurrentTime, utctDay)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import           Web.Telegram.API.Bot

import           Telegram.Ext

data Activity = Activity {
  times :: [Time],
  location :: T.Text,
  description :: T.Text,
  students :: T.Text } deriving ()

instance FromJSON Activity where
  parseJSON = withObject "Activity" $ \o -> do
    times <- o .: "Times"
    location <- o .: "LocationsJoined"
    description <- o .: "ModuleDescription"
    students <- o .: "StudentSetsJoined"
    return Activity{..}
instance Show Activity where
  show Activity{..} = T.unpack $ T.unlines $ [description, location] -- $ T.unlines [description, location, T.pack $ show times]
instance TShow Activity where
  tShow Activity{..} = T.unlines [T.concat (map tShow times), description, location]

class TShow a where
  tShow :: a -> T.Text

tPrint :: TShow a => a -> IO ()
tPrint a = T.putStrLn $ tShow a

data Time = Time {
  start :: T.Text,
  week :: Int,
  day :: Int} deriving (Eq)
instance FromJSON Time where
  parseJSON = withObject "Time" $ \o -> do
    start <- o .: "Start"
    week <- o .: "WeekNumber"
    day <- o .: "WeekDay"
    return Time{..}
instance Show Time where
  show x = show $ start x
instance TShow Time where
  tShow Time{..} = T.take 5 . T.drop 11 $ start

acts :: Value -> Parser [Activity]
acts = withObject "activity" $ \o -> o .: "Activities"  -- or just (.: "data")

timetableURL :: String -> String
timetableURL x = "http://rooster.rug.nl/api/Schedule?CourseCodes=" ++ x ++ "&language=EN&Locations=&Instructors=&StudentSetCodes=&LayoutMode=Wide&Mode=&intern=true"

get :: IO String
get = fmap (init . tail) $ simpleHTTP (getRequest $ timetableURL "Inf+B+1") >>= getResponseBody

timetable :: Action
timetable (chatId, _) = do
  x <- f
  case fmap (T.unlines . map tShow) x of
    Left x -> return $ sendMessageRequest (T.pack $ show chatId) "failed parsing is the CourseCode right?"
    Right "" -> return $ sendMessageRequest (T.pack $ show chatId) "No activities today"
    Right x  -> return $ sendMessageRequest (T.pack $ show chatId) (x)

f :: IO (Either T.Text [Activity])
f = do
  x <- get
  (_, week, day) <- getCurrentTime >>= return . toWeekDate . utctDay
  case parseEither acts =<< maybeToEither (decode (BL8.pack x)) :: Either String [Activity] of
    Left x -> return $ Left (T.pack x)
    Right x -> return . Right . filter (fltrGroups myGroups) . concatMap (fltrTime week day) $ x
  where
    maybeToEither Nothing = Left "failed parsing, is the coursecode right?"
    maybeToEither (Just x) = Right x
fltrGroups :: [T.Text] -> Activity -> Bool
fltrGroups groups acts = or $ ((==""):(T.isInfixOf <$> groups)) <*> [students acts]

fltrTime :: Int-> Int-> Activity -> [Activity]
fltrTime cweek cday act
  | times act == [] = []
  | (week . head . times) act == cweek && (day . head . times) act == cday =
    act {times = [(head . times) act]}: fltrTime cweek cday act{times = (tail . times) act}
  | otherwise = fltrTime cweek cday act {times = (tail. times) act}

myGroups :: [T.Text]
myGroups =
  [ "CS 3"
  , "Pre-master Informatica Hanze en Noordelijke Hogeschool"
  , "Pre-master Wiskunde RuG"]
