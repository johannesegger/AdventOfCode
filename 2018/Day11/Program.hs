import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import Data.Maybe (fromMaybe)
import Data.List (maximumBy)
import Data.Ord (comparing)

serialNumber :: Int
serialNumber = 3031

main :: IO ()
main = do
    print solve1
    print solve2

solve1 :: (Int, Int)
solve1 = fst $ maximumBy (comparing snd) $ calculateTotalPower <$> topLeftCells
    where
        topLeftCells = [ (x, y) | x <- [1..298], y <- [1..298] ]
        calculateTotalPower (x, y) =
            let totalPower = sum $ getPower <$> getSquare 3 (x, y) in
            ((x, y), totalPower)
        getSquare size (x, y) = [ (x + dx, y + dy) | dx <- [0..size - 1], dy <- [0..size - 1] ]

solve2 :: (((Int, Int), Int), Int)
solve2 = maximumBy (comparing snd) $ calculateMaxTotalPowerFromCell <$> cells
    where
        totalSize = 300
        cells = [ (x, y) | x <- [1..totalSize], y <- [1..totalSize] ]
        powerMap = foldl updateSummedAreaTable Map.empty cells
        updateSummedAreaTable m (x, y) =
            let cellPower = getPower (x, y)
                sum1 = fromMaybe 0 $ m !? (x, y - 1)
                sum2 = fromMaybe 0 $ m !? (x - 1, y)
                sum3 = fromMaybe 0 $ m !? (x - 1, y - 1)
                power = cellPower + sum1 + sum2 - sum3
            in
                Map.insert (x, y) power m
        calculateMaxTotalPowerFromCell (x, y) =
            let (size, power) = maximumBy (comparing snd) $ calculateTotalPower (x, y) <$> [1..totalSize - max x y + 1] in
            (((x, y), size), power)
        calculateTotalPower (startX, startY) size =
            let leftTopPower = fromMaybe 0 $ powerMap !? (startX - 1, startY - 1)
                rightTopPower = fromMaybe 0 $ powerMap !? (startX + size - 1, startY - 1)
                leftBottomPower = fromMaybe 0 $ powerMap !? (startX - 1, startY + size - 1)
                rightBottomPower = fromMaybe 0 $ powerMap !? (startX + size - 1, startY + size - 1)
            in (size, rightBottomPower - leftBottomPower - rightTopPower + leftTopPower)

getPower :: (Int, Int) -> Int
getPower (x, y) =
    let rackId = x + 10
        powerLevel = rackId * y
        powerLevel' = powerLevel + serialNumber
        powerLevel'' = powerLevel' * rackId
        hundredsDigit = powerLevel'' `div` 100 `mod` 10
    in
        hundredsDigit - 5