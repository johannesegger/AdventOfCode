main :: IO ()
main = do
    input <- readInput
    putStrLn $ show $ solve1 input
    putStrLn $ show $ solve2 input

type Metadata = Int
data Tree = Node [Metadata] [Tree] deriving Show

readInput :: IO Tree
readInput = parseTree . map read . words <$> readFile "input.txt"

parseTree :: [Int] -> Tree
parseTree input = snd $ parseChildNode input
    where
        parseChildNode (numberOfChildren : numberOfMetadataEntries : xs) =
            let (xs', children) = parseChildNodes numberOfChildren xs
                (metadataEntries, xs'') = splitAt numberOfMetadataEntries xs'
            in
            (xs'', Node metadataEntries children)

        parseChildNodes count input = go [] count input
            where
                go acc 0 input = (input, reverse acc)
                go acc count input =
                    let (input', node) = parseChildNode input in
                    go (node:acc) (count - 1) input'

solve1 :: Tree -> Int
solve1 tree = sumMetadataEntries tree
    where
        sumMetadataEntries (Node metadataEntries children) =
            sum metadataEntries + (sum $ map sumMetadataEntries children)

solve2 :: Tree -> Int
solve2 tree = sumNode tree
    where
        sumNode (Node metadataEntries []) = sum metadataEntries
        sumNode (Node indexes children) = sum $ map (getNodeValueByIndex . (\idx -> idx - 1)) indexes
            where
                getNodeValueByIndex idx
                    | idx >= 0 && idx < length children = sumNode $ children !! idx
                    | otherwise = 0