import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (tails, find)
import Data.Maybe (mapMaybe, listToMaybe)

main :: IO ()
main = do
    input <- readInput
    putStrLn $ show $ solve1 input
    putStrLn $ show $ solve2 input

readInput :: IO [String]
readInput = lines <$> readFile "input.txt"

solve1 :: [String] -> Int
solve1 input = (occurrences 2) * (occurrences 3)
    where
        occurrences count = sum $ map (boolToInt . elem count) allOccurrences
        allOccurrences = map (getOccurrences Map.empty) input
        getOccurrences m [] = map snd $ Map.toList m
        getOccurrences m (c : xs) =
            let m' = Map.alter insertOrUpdateOccurrence c m in
            getOccurrences m' xs
        insertOrUpdateOccurrence (Just v) = Just $ v + 1
        insertOrUpdateOccurrence Nothing = Just 1
        boolToInt False = 0
        boolToInt True = 1

solve2 :: [String] -> Maybe String
solve2 input = listToMaybe $ mapMaybe getIntersectionWithOneLetterDiff allPairs
    where
        allPairs = [ (x, y) | x:ys <- tails input, y <- ys ]
        getIntersectionWithOneLetterDiff (a, b)
            | length intersection == length a - 1 = Just intersection
            | otherwise = Nothing
            where
                intersection = mapMaybe intersectCharacter $ zip a b
                intersectCharacter (a, b)
                    | a == b = Just a
                    | otherwise = Nothing
