import Data.List (tails, find, group, sort)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Bool (bool)

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
        occurrences count = sum $ map (bool 0 1 . elem count) allOccurrences
        allOccurrences = map getOccurrences input
        getOccurrences = map length . group . sort

solve2 :: [String] -> Maybe String
solve2 input = listToMaybe $ mapMaybe getIntersectionWithOneLetterDiff allPairs
    where
        allPairs = [ (x, y) | x:ys <- tails input, y <- ys ]
        getIntersectionWithOneLetterDiff (a, b)
            | length intersection == length a - 1 = Just intersection
            | otherwise = Nothing
            where
                intersection = zippedIntersection a b

zippedIntersection :: Eq a => [a] -> [a] -> [a]
zippedIntersection a b = [ a' | (a', b') <- zip a b, a' == b' ]
