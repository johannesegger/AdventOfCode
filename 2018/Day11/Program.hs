{-# LANGUAGE TupleSections #-}

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

solve2 :: (((Int, Int), Int), Int)
solve2 = maximumBy (comparing snd) $ calculateMaxTotalPowerFromCell <$> topLeftCells
    where
        totalSize = 300
        topLeftCells = [ (x, y) | x <- [1..totalSize], y <- [1..totalSize] ]
        calculateMaxTotalPowerFromCell (x, y) =
            let (size, power) = maximumBy (comparing snd) $ scanl (calculateTotalPower (x, y)) (0, 0) $ replicate (totalSize - max x y + 1) () in
            (((x, y), size), power)
        calculateTotalPower (startX, startY) (size, power) () =
            let horizontalPower = sum $ getPower . (, startY + size) <$> [startX..startX + size]
                verticalPower = sum $ getPower . (startX + size, ) <$> [startY..startY + size - 1]
            in (size + 1, power + horizontalPower + verticalPower)

getSquare :: Int -> (Int, Int) -> [(Int, Int)]
getSquare size (x, y) = [ (x + dx, y + dy) | dx <- [0..size - 1], dy <- [0..size - 1] ]

getPower :: (Int, Int) -> Int
getPower (x, y) =
    let rackId = x + 10
        powerLevel = rackId * y
        powerLevel' = powerLevel + serialNumber
        powerLevel'' = powerLevel' * rackId
        hundredsDigit = powerLevel'' `div` 100 `mod` 10
    in
        hundredsDigit - 5