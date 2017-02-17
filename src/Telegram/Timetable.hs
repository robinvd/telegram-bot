{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Timetable (timetable, timetableRegister) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8 (pack)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time (getCurrentTime, utctDay)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import           GHC.Generics
import           Web.Telegram.API.Bot

import           Telegram.Ext

-- | data types that are mainly used for automatic parsing of the JSON input
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

-- | same as show but now for T.Text
class TShow a where
  tShow :: a -> T.Text

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

-- | helper function so not needed info is discarded from the JSON
parseHelper :: Value -> Parser [Activity]
parseHelper = withObject "activity" $ \o -> o .: "Activities"

-- | types and functions for storing the preferences, currently reads the file everytime!
type Database = M.Map Int Entry

data Entry = Entry {
  courseCode :: T.Text,
  groups :: [T.Text] } deriving (Generic, Ord, Eq)
instance ToJSON Entry
instance FromJSON Entry

readDatabase :: IO Database
readDatabase = do
  x <- decode <$> BL.readFile "database" :: IO (Maybe Database)
  case x of
    Just a -> return a
    Nothing -> error "Corrupt database!"

writeDatabase :: Database -> IO ()
writeDatabase = BL.writeFile "database" . encode

-- | function for fetching the JSON file
timetableURL :: String -> String
timetableURL x = "http://rooster.rug.nl/api/Schedule?CourseCodes=" ++ x ++ "&language=EN&Locations=&Instructors=&StudentSetCodes=&LayoutMode=Wide&Mode=&intern=true"

-- | issues a get request to the rug server
-- | (TODO) use Data.Http.Client so we have one less import
get :: String -> IO String
get courseCode = simpleHTTP (getRequest $ timetableURL courseCode) >>= getResponseBody

-- | stores someones preferences in the database
timetableRegister :: Action
timetableRegister (chatId, _:courseCode:unparsedGroups) = do
  readDatabase >>= writeDatabase . M.insert chatId Entry {..}
  return $ sendMessageRequest (T.pack $ show chatId) "done"
  where
    groups :: [T.Text]
    groups = T.splitOn "," . T.unwords $ unparsedGroups
timetableRegister (chatId, _) = return $ sendMessageRequest (T.pack $ show chatId) "please input cousecode and groups. See \"/help timetableRegister for info\""

-- | main function that is called on "/timetable"\
-- | could be merged with getMessage
timetable :: Action
timetable (chatId, _) = do
  database <- readDatabase
  case M.lookup chatId database of
    Nothing -> return $ sendMessageRequest (T.pack $ show chatId) "please register fist with /tr"
    Just entry -> getMessage chatId entry

getMessage :: Int -> Entry -> IO SendMessageRequest
getMessage chatId entry = do
  activities <- getActivities entry
  case fmap (T.unlines . map tShow) activities of
    Left _ -> return $ sendMessageRequest (T.pack $ show chatId) "failed parsing is the CourseCode right?"
    Right "" -> return $ sendMessageRequest (T.pack $ show chatId) "No activities today"
    Right x  -> return $ sendMessageRequest (T.pack $ show chatId) (x)

-- | functions below are a bit hacky
-- | They parse and filter the JSON to only the relevant Activies
getActivities :: Entry -> IO (Either T.Text [Activity])
getActivities Entry{..} = do
  fullJson <- get $ T.unpack courseCode
  (_, week, day) <- toWeekDate . utctDay <$> getCurrentTime
  case mapMaybe (parseMaybe parseHelper) . fromJust . decode . BL8.pack $ fullJson of
    [] -> return $ Left "error"
    x  -> return . Right . filter (fltrGroups groups) . concatMap (fltrTime week day) $ concat x

fltrGroups :: [T.Text] -> Activity -> Bool
fltrGroups groups acts = or $ ((==""):(T.isInfixOf <$> groups)) <*> [students acts]

fltrTime :: Int-> Int-> Activity -> [Activity]
fltrTime cweek cday act
  | times act == [] = []
  | (week . head . times) act == cweek && (day . head . times) act == cday =
    act {times = [(head . times) act]}: fltrTime cweek cday act{times = (tail . times) act}
  | otherwise = fltrTime cweek cday act {times = (tail. times) act}
