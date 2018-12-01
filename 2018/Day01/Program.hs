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
readSignedInt ('-' : number) = -(read number)

solution1 :: [Int] -> Int
solution1 = sum

solution2 :: [Int] -> Int
solution2 numbers = fromJust $ fmap fst $ find (uncurry Set.member) $ scanl fn (0, Set.empty) $ cycle numbers
    where
        fn (frequency, frequencies) n =
            (frequency + n, Set.insert frequency frequencies)