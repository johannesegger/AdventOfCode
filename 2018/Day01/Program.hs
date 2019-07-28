import qualified Data.Set as Set
import Data.List (find)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    input <- readInput
    print $ solution1 input
    print $ solution2 input

readInput :: IO [Int]
readInput = do
    input <- readFile "input.txt"
    return $ map readSignedInt $ lines input

readSignedInt :: String -> Int
readSignedInt ('+' : number) = read number
readSignedInt ('-' : number) = negate $ read number
readSignedInt _ = error "Number must start with '+' or '-'"

solution1 :: [Int] -> Int
solution1 = sum

solution2 :: [Int] -> Int
solution2 numbers = fst $ fromJust $ find hasFrequency $ getFrequencies $ cycle numbers
    where
        hasFrequency = uncurry Set.member
        initialFrequency = (0, Set.empty)
        getFrequencies = scanl updateFrequencies initialFrequency
        updateFrequencies (frequency, frequencies) number =
            (frequency + number, Set.insert frequency frequencies)