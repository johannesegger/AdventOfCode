import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (find)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    input <- readInput
    putStrLn $ show $ solution1 input
    putStrLn $ show $ solution2 input

readInput :: IO [Int]
readInput = do
    input <- readFile "input.txt"
    return $ map readSignedInt $ lines input

readSignedInt :: String -> Int
readSignedInt ('+' : number) = read number
readSignedInt ('-' : number) = negate $ read number

solution1 :: [Int] -> Int
solution1 = sum

solution2 :: [Int] -> Maybe Int
solution2 numbers = fmap fst $ find hasFrequency $ getFrequencies $ cycle numbers
    where
        hasFrequency = uncurry Set.member
        initialFrequency = (0, Set.empty)
        getFrequencies = scanl updateFrequencies initialFrequency
        updateFrequencies (frequency, frequencies) number =
            (frequency + number, Set.insert frequency frequencies)