{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Telegram.Timetable (timetable, timetableRegister) where

import           Control.Concurrent
import           Control.Monad.Writer
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8 (pack)
import           Data.List (sort)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Time (getCurrentTime, utctDay)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           GHC.Generics
import           Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import           Web.Telegram.API.Bot

import           Telegram.Ext

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

(!!?) :: [a] -> Int -> Maybe a
(x:_) !!? 0 = Just x
[] !!? 0 = Nothing
(_:xs) !!? n = xs !!? (n-1)

zipList :: [a] -> [a] -> [a]
zipList [] _ = []
zipList _ [] = []
zipList (x:xs) (y:ys) = x:y:zipList xs ys


-- | data types that are mainly used for automatic parsing of the JSON input
data Course = Course {
  courseCode :: T.Text,
  courseName :: T.Text,
  activities :: [Activity]}
instance TShow Course where
  tShow Course{..} = T.unlines (courseName: map tShow activities)

data Activity = Activity {
  times :: [Time],
  location :: T.Text,
  description :: T.Text,
  activityTypeDescription :: T.Text,
  students :: T.Text } deriving (Eq)
instance Ord Activity where
  compare Activity {times = times1} Activity {times = times2} = compare times1 times2

instance FromJSON Activity where
  parseJSON = withObject "Activity" $ \o -> do
    times <- o .: "Times"
    location <- o .: "LocationsJoined"
    description <- o .: "ModuleDescription"
    activityDescription <- o .: "ActivityDescription"
    activityTypeDescription <- o .: "ActivityTypeDescription"
    studentSets <- o .: "StudentSetsJoined"
    let students = activityDescription `T.append` studentSets
    return Activity{..}
instance Show Activity where
  show Activity{..} = T.unpack $ T.unlines [description, location]
instance TShow Activity where
  tShow Activity{..} = T.unlines [T.concat (map tShow times ++ [" - "] ++ [activityTypeDescription]), description, location]

-- | same as show but now for T.Text
class TShow a where
  tShow :: a -> T.Text

data Time = Time {
  week :: Int,
  day :: Int,
  start :: T.Text} deriving (Eq, Ord)
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

-- | types and functions for storing the preferences
type Database = M.Map ChatId [Entry]
instance ToJSONKey ChatId
instance FromJSONKey ChatId
deriving instance Eq ChatId
deriving instance Ord ChatId

data Entry = Entry {
  entryCourseCode :: T.Text,
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
-- | (TODO) make everything use T.Text
get :: String -> IO String
get courseCode = simpleHTTP (getRequest $ timetableURL courseCode) >>= getResponseBody



-- | main function
-- | registers 2 commands "/t" and "/tr"
-- | (TODO) should use TVar instead of MVar
-- | (TODO) use an actual database instead of a json file
timetable :: Setup
timetable = do
  dbRef <- liftIO $ newMVar =<< readDatabase
  mapM_ (registerAction $ timetableMain dbRef) ["/t", "/timetable"]
  mapM_ (registerAction $ timetableWeek dbRef) ["/w", "/week"]
  mapM_ (registerAction $ timetableRegister dbRef) ["/tr", "/timetableRegister","timetableregister"]
  registerAction (timetableReset dbRef) "reset"
  registerCleanUp (readMVar dbRef >>= writeDatabase)



-- | stores someones preferences in the database
timetableRegister :: MVar Database -> Action
timetableRegister dbRef (chatId, _:entryCourseCode:unparsedGroups) = do
  modifyMVar dbRef (return . (\x -> (x,())) . M.insertWith (++) chatId [Entry {..}])
  return $ sendMessageRequest chatId "done"
  where
    groups :: [T.Text]
    groups = T.splitOn "," . T.unwords $ unparsedGroups
timetableRegister _ (chatId, _) = return $ sendMessageRequest chatId
  "please input cousecode and groups. See \"/help timetableRegister\" for info\""



timetableReset :: MVar Database -> Action
timetableReset dbRef (chatId, _) = do
  modifyMVar dbRef (return . (\x -> (x,())) . M.delete chatId)
  return $ sendMessageRequest chatId "done"



-- | function that gets called on "/t"
-- | could be merged with getMessage
-- | (TODO) clean this up a bit
timetableMain :: MVar Database -> Action
timetableMain dbRef (chatId, xs) = do
  date <- (\(y, w, d) -> (fromInteger y, w + arg `div` 7, (d + arg) `mod` 7)) . toWeekDate . utctDay <$> getCurrentTime
  database <- readMVar dbRef
  case M.lookup chatId database of
    Nothing -> return $ sendMessageRequest chatId
      "please register fist with /tr"
    Just entries -> sendMessageRequest chatId <$> getMessage date entries
  where
    arg :: Int
    arg = parse xs
    parse :: [T.Text] -> Int
    parse xs = fromMaybe 0 $ Just . fst =<< eitherToMaybe . T.signed T.decimal =<< xs !!? 1


-- | (TODO) should not use head entry, as it is partial
-- | (TODO) display if an course failed to parse
getMessage :: (Int, Int, Int) -> [Entry] -> IO T.Text
getMessage date entries = do
  activities <- mapM (getActivities date) entries :: IO [Maybe [Activity]]
  case (T.unlines . map tShow . sort. concat . catMaybes) activities of
    --Nothing -> return $ sendMessageRequest (T.pack $ show chatId)
    --  "failed parsing is the CourseCode right?"
    "" -> return "No activities today"
    x  -> return  x



timetableWeek :: MVar Database -> Action
timetableWeek dbRef (chatId, xs) = do
  (y, w, _) <- (\(y, w, d) -> (fromInteger y, w + arg `div` 7, (d + arg) `mod` 7)) . toWeekDate . utctDay <$> getCurrentTime
  database <- readMVar dbRef
  case M.lookup chatId database of
    Nothing -> return $ sendMessageRequest chatId
      "please register fist with /tr"
    Just entries -> sendMessageRequest chatId .
      T.unlines . zipList daysOfTheWeek <$> mapM (`getMessage` entries) [(y,w,x) | x <- [1..5]]
  where
    arg :: Int
    arg = parse xs
    parse :: [T.Text] -> Int
    parse xs = fromMaybe 0 $ Just . fst =<< eitherToMaybe . T.signed T.decimal =<< xs !!? 1
    daysOfTheWeek = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]



-- | functions below are a bit hacky
-- | They parse and filter the JSON to only the relevant Activies
getActivities :: (Int, Int, Int) -> Entry -> IO (Maybe [Activity])
getActivities (_, week, day) Entry{..} = do
  fullJson <- BL8.pack <$> get (T.unpack entryCourseCode)
  case mapMaybe (parseMaybe parseHelper) . fromMaybe [] . decode $ fullJson of
    [] -> return Nothing
    x  -> return . Just . filter (fltrGroups groups) . concatMap (fltrTime week day) $ concat x

fltrGroups :: [T.Text] -> Activity -> Bool
fltrGroups groups acts = or $ ((==""):(T.isInfixOf <$> groups)) <*> [students acts]

fltrTime :: Int-> Int-> Activity -> [Activity]
fltrTime cweek cday act
  | null $ times act = []
  | (week . head . times) act == cweek && (day . head . times) act == cday =
    act {times = [(head . times) act]}: fltrTime cweek cday act{times = (tail . times) act}
  | otherwise = fltrTime cweek cday act {times = (tail. times) act}
