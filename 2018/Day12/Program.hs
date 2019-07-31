import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec

main :: IO ()
main = do
    input <- readInput
    print $ solve1 input
    print $ solve2 input

type NextGenPlantPattern = [Bool]
type PotsWithPlants = Set Int

readInput :: IO (PotsWithPlants, Set NextGenPlantPattern)
readInput = either (error . show) filterPatterns . parse parser "input.txt" <$> readFile "input.txt"
    where
        parser = do
            string "initial state: "
            state <- indexSet <$> many (noPlant <|> yesPlant)
            endOfLine
            endOfLine
            patterns <- sepEndBy1 patternParser endOfLine
            return (state, patterns)
        noPlant = False <$ char '.'
        yesPlant = True <$ char '#'
        patternParser = do
            plantPattern <- many (noPlant <|> yesPlant)
            string " => "
            nextGenPlant <- noPlant <|> yesPlant
            return (plantPattern, nextGenPlant)
        indexSet x =
            let fn i (True : xs) acc = fn (i + 1) xs $ Set.insert i acc
                fn i (False : xs) acc = fn (i + 1) xs acc
                fn _ [] acc = acc
            in
                fn 0 x Set.empty
        filterPatterns (potsWithPlants, nextGenPlantPatterns) =
            let patterns = Set.fromList $ fst <$> filter snd nextGenPlantPatterns in
            (potsWithPlants, patterns)

solve1 :: (PotsWithPlants, Set NextGenPlantPattern) -> Int
solve1 (potsWithPlants, patternsForNextGenPlants) = sum $ foldl nextGeneration' potsWithPlants $ replicate 20 ()
    where
        nextGeneration' = nextGeneration patternsForNextGenPlants

solve2 :: (PotsWithPlants, Set NextGenPlantPattern) -> Int
solve2 (potsWithPlants, patternsForNextGenPlants) = sumFirstGeneration + sumFirstDiffs + sumCycles
    where
        nextGeneration' = nextGeneration patternsForNextGenPlants
        pairs xs = zip xs (tail xs)
        diff (a, b) = b - a
        diffs = diff <$> pairs (sum <$> scanl nextGeneration' potsWithPlants (replicate 1000 ()))
        sumFirstGeneration = sum potsWithPlants
        sumFirstDiffs = sum diffs
        sumCycles = (50000000000 - 1000) * last diffs

nextGeneration :: Set NextGenPlantPattern -> PotsWithPlants -> () -> PotsWithPlants
nextGeneration patternsForNextGenPlants potsWithPlants () =
    let minPotWithPlant = Set.findMin potsWithPlants
        maxPotWithPlant = Set.findMax potsWithPlants
        patternLength = 5
        potsWithPatterns = potWithPattern <$> [minPotWithPlant - patternLength `div` 2 ..maxPotWithPlant + patternLength `div` 2]
        potWithPattern i =
            let p = flip Set.member potsWithPlants <$> [i - patternLength `div` 2 .. i + patternLength `div` 2] in
            (i, p)
        hasNextGenerationPlant (_i, p) = Set.member p patternsForNextGenPlants
    in
        Set.fromDistinctAscList $ fst <$> filter hasNextGenerationPlant potsWithPatterns
