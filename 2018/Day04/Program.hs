import Text.Parsec
import Data.Time
import Data.Time.Clock
import Data.List (group, sort, sortOn, maximumBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, unionWith, singleton)
import Data.Ord (comparing)

main :: IO ()
main = do
    input <- readInput
    putStrLn $ show $ solve1 input
    putStrLn $ show $ solve2 input

readInput :: IO [LogEntry]
readInput = sortOn timestamp . map (either (error . show) id . parseLogEntry) . lines <$> readFile "input.txt"

type GuardId = Int
data LogEntry = LogEntry { timestamp :: UTCTime, event :: GuardEvent } deriving (Show)
data GuardEvent = BeginsShift GuardId | WakesUp | FallsAsleep deriving (Show)

parseLogEntry :: String -> Either ParseError LogEntry
parseLogEntry = parse parseLogEntry "input.txt"
    where
        parseLogEntry = LogEntry <$> parseTimestamp <*> (spaces *> parseEvent)
        parseTimestamp = UTCTime <$> (char '[' *> parseDate <* spaces) <*> (realToFrac <$> parseTime <* char ']')
        parseDate = fromGregorian <$> (toInteger <$> parseNumber) <*> (char '-' *> parseNumber) <*> (char '-' *> parseNumber)
        parseTime = (+) <$> ((*) 3600 <$> parseNumber) <*> (char ':' *> ((*) 60 <$> parseNumber))
        parseNumber = read <$> many1 digit
        parseEvent = parseGuardBeginsShift <|> parseWakesUp <|> parseFallsAsleep
        parseWakesUp = const WakesUp <$> string "wakes up"
        parseFallsAsleep = const FallsAsleep <$> string "falls asleep"
        parseGuardBeginsShift = BeginsShift <$> (string "Guard #" *> parseNumber <* string " begins shift")

type GuardSleepTimes = Map GuardId [UTCTime]

solve1 :: [LogEntry] -> Int
solve1 entries = guardId * maxSleepMinute
    where
        (guardId, sleepTimes) = maximumBy (comparing (length . snd)) $ Map.toList $ getSleepTimes Nothing entries
        maxSleepMinute = head $ maximumBy (comparing length) $ group $ sort $ map getMinute $ sleepTimes

solve2 :: [LogEntry] -> Int
solve2 entries = guardId * maxSleepMinute
    where
        (guardId, sleepTimes) = maximumBy (comparing (map length . snd)) $ Map.toList $ Map.map (group . sort . map getMinute) $ getSleepTimes Nothing entries
        maxSleepMinute = head $ maximumBy (comparing length) sleepTimes

getMinute :: UTCTime -> Int
getMinute utcTime =
    let TimeOfDay hours minutes seconds = timeToTimeOfDay (utctDayTime utcTime) in
    minutes

getSleepTimes :: Maybe GuardId -> [LogEntry] -> GuardSleepTimes
getSleepTimes _ (LogEntry { event = BeginsShift guardId } : xs) =
    getSleepTimes (Just guardId) xs
getSleepTimes (Just guardId) (LogEntry { timestamp = t1, event = FallsAsleep } : LogEntry { timestamp = t2, event = WakesUp } : xs) =
    Map.unionWith mappend nextSleepTimes sleepTimes
    where
        nextSleepTimes = getSleepTimes (Just guardId) xs
        guardSleepTimes = Map.singleton guardId ()
        sleepTimes = Map.singleton guardId $ minutesBetween t1 t2
getSleepTimes _ [] = Map.empty

minutesBetween :: UTCTime -> UTCTime -> [UTCTime]
minutesBetween t1 t2 = [ addUTCTime x t1 | x <- [ realToFrac 0, realToFrac 60 .. t2 `diffUTCTime` t1 - realToFrac 60 ] ]
