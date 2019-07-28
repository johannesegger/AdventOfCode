import Data.List (group, groupBy, nub, sort, sortOn)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    input <- readInput
    print $ solve1 input
    print $ solve2 input

type Coordinate = (Int, Int)

readInput :: IO [Coordinate]
readInput = map (read . (\l -> "(" ++ l ++ ")")) . lines <$> readFile "input.txt"

solve1 :: [Coordinate] -> Int
solve1 coordinates = maximum $ map length $ group $ sort $ filterInfiniteCoordinates $ mapMaybe (getClosestCoordinate coordinates) $ areaCoordinates coordinates
    where
        infiniteCoordinates = getInfiniteCoordinates coordinates
        filterInfiniteCoordinates = filter (`notElem` infiniteCoordinates)

solve2 :: [Coordinate] -> Int
solve2 coordinates = length $ filter (< 10000) $ map (getSumOfDistances coordinates) $ areaCoordinates coordinates

areaCoordinates :: [Coordinate] -> [Coordinate]
areaCoordinates coordinates = [ (x, y) | x <- [x1 .. x2], y <- [y1 .. y2] ]
    where
        ((x1, y1), (x2, y2)) = getBounds coordinates

getBorderCoordinates :: [Coordinate] -> [Coordinate]
getBorderCoordinates coordinates =
    [ (x, y) | x <- [x1..x2], y <- [y1, y2] ] ++
    [ (x, y) | x <- [x1, x2], y <- [y1..y2] ]
    where
        ((x1, y1), (x2, y2)) = getBounds coordinates

getInfiniteCoordinates :: [Coordinate] -> [Coordinate]
getInfiniteCoordinates coordinates = nub $ mapMaybe (getClosestCoordinate coordinates) $ getBorderCoordinates coordinates

getClosestCoordinate :: [Coordinate] -> Coordinate -> Maybe Coordinate
getClosestCoordinate coordinates coordinate =
    case groupBy (\a b -> snd a == snd b) $ sortOn snd $ map (\c -> (c, getDistance c coordinate)) coordinates of
        [item]:_ -> Just $ fst item
        _ -> Nothing

getSumOfDistances :: [Coordinate] -> Coordinate -> Int
getSumOfDistances coordinates coordinate = sum $ map (getDistance coordinate) coordinates

getDistance :: Coordinate -> Coordinate -> Int
getDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

getBounds :: [Coordinate] -> (Coordinate, Coordinate)
getBounds coordinates = ((minX, minY), (maxX, maxY))
    where
        minX = minimum $ map fst coordinates
        minY = minimum $ map snd coordinates
        maxX = maximum $ map fst coordinates
        maxY = maximum $ map snd coordinates