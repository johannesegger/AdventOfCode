import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Text.Parsec

main :: IO ()
main = do
    input <- readInput
    print $ solve 20 input
    print $ solve 5000 input

type NextGenPlantPattern = Set Int
type PotsWithPlants = Set Int

readInput :: IO (PotsWithPlants, [NextGenPlantPattern])
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
            plantPattern <- indexSet <$> many (noPlant <|> yesPlant)
            string " => "
            nextGenPlant <- noPlant <|> yesPlant
            return (plantPattern, nextGenPlant)
        indexSet x =
            let fn i (True : xs) acc = fn (i + 1) xs $ Set.insert i acc
                fn i (False : xs) acc = fn (i + 1) xs $ acc
                fn _ [] acc = acc
            in
                fn 0 x Set.empty
        filterPatterns (potsWithPlants, nextGenPlantPatterns) =
            let patterns = fst <$> filter snd nextGenPlantPatterns in
            (potsWithPlants, patterns)

solve :: Int -> (PotsWithPlants, [NextGenPlantPattern]) -> Int
solve iterations (initialPotsWithPlants, patternsForNextGenPlants) = sum $ foldl nextGeneration initialPotsWithPlants $ replicate iterations ()
    where
        nextGeneration potsWithPlants () =
            let minPotWithPlant = Set.findMin potsWithPlants
                maxPotWithPlant = Set.findMax potsWithPlants
                patternLength = 5
                offsets = [minPotWithPlant - patternLength `div` 2..maxPotWithPlant + patternLength `div` 2]
                hasNextGenerationPlant offset = any (isPatternMatch offset) patternsForNextGenPlants
                isPatternMatch offset p =
                    let plantSet = offsetPattern offset p
                        noPlantSet = offsetPattern offset $ Set.fromAscList [0..patternLength - 1] \\ p
                    in
                        plantSet `Set.isSubsetOf` potsWithPlants &&
                        Set.disjoint noPlantSet potsWithPlants
                offsetPattern offset = Set.map (+ (offset - patternLength `div` 2))
            in
                Set.fromDistinctAscList $ filter hasNextGenerationPlant offsets