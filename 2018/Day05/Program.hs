import Data.Char (toLower)

main :: IO ()
main = do
    input <- readInput
    print $ solve1 input
    print $ solve2 input

type UnitType = Char
type Polymer = [UnitType]

readInput :: IO Polymer
readInput = readFile "input.txt"

solve1 :: Polymer -> Int
solve1 input = length $ react [] input

solve2 :: Polymer -> Int
solve2 input = minimum $ map length [ react [] $ removeUnitType unitType input | unitType <- ['a'..'z'] ]

react :: Polymer -> Polymer -> Polymer
react [] (a:b:xs)
    | isReacting a b = react [] xs
    | otherwise = react [a] (b:xs)
react [] [a] = [a]
react (x:xs) (y:ys)
    | isReacting x y = react xs ys
    | otherwise = react (y:x:xs) ys
react acc [] = reverse acc

isReacting :: UnitType -> UnitType -> Bool
isReacting a b = a /= b && toLower a == toLower b

removeUnitType :: UnitType -> Polymer -> Polymer
removeUnitType unitType = filter (\c -> toLower c /= unitType)
