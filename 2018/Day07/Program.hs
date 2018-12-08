import Data.Char (ord)
import Data.List (sort)
import Data.Maybe (fromJust, isNothing, maybe, listToMaybe)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec

main :: IO ()
main = do
    input <- readInput
    putStrLn $ show $ solve1 input
    putStrLn $ show $ solve2 input

type Step = Char
type Instruction = (Step, (Set Step))

readInput :: IO [Instruction]
readInput = Map.toList . foldl (Map.unionWith Set.union) Map.empty . concatMap ((\(x, y) -> [ Map.singleton y $ Set.singleton x, Map.singleton x Set.empty ] ) . either (error . show) id . parseInstruction) . lines <$> readFile "input.txt"

parseInstruction :: String -> Either ParseError (Step, Step)
parseInstruction = parse instructionParser "input.txt"
    where
        instructionParser = do
            string "Step "
            a <- letter
            string " must be finished before step "
            b <- letter
            string " can begin."
            return (a, b)

solve1 :: [Instruction] -> [Step]
solve1 instructions = reverse $ go [] instructions
    where
        go executedSteps [] = executedSteps
        go executedSteps remainingInstructions = go (nextStep : executedSteps) remainingInstructions'
            where
                nextStep = fromJust $ findNextStep remainingInstructions
                remainingInstructions' = setStepFinished nextStep $ setStepStarted nextStep remainingInstructions

solve2 :: [Instruction] -> Int
solve2 instructions = go 0 (const Nothing <$> [1..5]) instructions
    where
        go time workers []
            | all isNothing workers = time - 1
        go time workers instructions = go time' workers'' instructions''
            where
                time' = time + 1
                (instructions', workers') = foldr doWork (instructions, []) workers
                (instructions'', workers'') = foldr startNewWork (instructions', []) workers'

doWork :: Maybe (Step, Int) -> ([Instruction], [Maybe (Step, Int)]) -> ([Instruction], [Maybe (Step, Int)])
doWork Nothing (instr, workers) =
    (instr, Nothing:workers)
doWork (Just (step, 1)) (instr, workers) =
    let instr' = setStepFinished step instr in
    (instr', Nothing:workers)
doWork (Just (step, time)) (instr, workers) =
    (instr, Just (step, time - 1):workers)

startNewWork :: Maybe (Step, Int) -> ([Instruction], [Maybe (Step, Int)]) -> ([Instruction], [Maybe (Step, Int)])
startNewWork Nothing (instr, workers) =
    case findNextStep instr of
        Just step -> (setStepStarted step instr, Just (step, getStepTime step):workers)
        Nothing -> (instr, Nothing:workers)
startNewWork worker (instr, workers) =
    (instr, worker:workers)

getStepTime :: Step -> Int
getStepTime step = 60 + (ord step - ord 'A' + 1)

findNextStep :: [Instruction] -> Maybe Step
findNextStep instructions =
    listToMaybe $ sort $ map fst $ filter (null . snd) instructions

setStepStarted :: Step -> [Instruction] -> [Instruction]
setStepStarted step instructions =
    filter (not . (==) step . fst) instructions

setStepFinished :: Step -> [Instruction] -> [Instruction]
setStepFinished step instructions =
    map (\(s, deps) -> (s, Set.delete step deps)) instructions
