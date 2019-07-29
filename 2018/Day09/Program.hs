{-# LANGUAGE TupleSections #-}

import Data.Map.Strict (Map, fromList, adjust, (!), insert, delete)
import Text.Parsec

main :: IO ()
main = do
    (players, marbles) <- readInput
    print $ solve players marbles
    print $ solve players (marbles * 100)

readInput :: IO (Int, Int)
readInput = either (error . show) id . parse parser "input.txt" <$> readFile "input.txt"
    where
        parser = do
            players <- read <$> many1 digit
            string " players; last marble is worth "
            marbles <- read <$> many1 digit
            string " points"
            return (players, marbles)

type Field = Map Int (Int, Int)

data GameState = GameState { currentMarble :: Int
                   , scores :: Map Int Integer
                   , field :: Field
                   }

solve :: Int -> Int -> Integer
solve players marbles =
    maximum $ scores $ foldl play initialState [1..marbles]
    where
        initialScores = fromList $ fmap (, 0) [0..players-1]
        initialField = fromList $ [(0, (0, 0))]
        initialState = GameState 0 initialScores initialField

play :: GameState -> Int -> GameState
play state marble
    | marble `rem` 23 == 0 = playSpecial state marble
    | otherwise = playNormal state marble

playNormal :: GameState -> Int -> GameState
playNormal state marble = state { currentMarble = marble, field = field' }
    where
        (nextMarble, (_, nextMarbleRight)) = move (field state) (currentMarble state) 1
        field' =
            adjust (\(l, _) -> (l, marble)) nextMarble . 
            adjust (\(_, r) -> (marble, r)) nextMarbleRight .
            insert marble (nextMarble, nextMarbleRight) $
            field state

playSpecial :: GameState -> Int -> GameState
playSpecial state marble = GameState currentMarble' playerScores' field'
    where
        playerScores = scores state
        playerIndex = marble `mod` length playerScores
        (marble2, (marble2Left, marble2Right)) = move (field state) (currentMarble state) (-7)
        field' =
            adjust (\(l, _) -> (l, marble2Right)) marble2Left .
            adjust (\(_, r) -> (marble2Left, r)) marble2Right .
            delete marble2 $
            field state
        currentMarble' = marble2Right
        playerScores' = adjust (\x -> x + toInteger marble + toInteger marble2) playerIndex playerScores

move :: Field -> Int -> Int -> (Int, (Int, Int))
move fld marble steps
    | steps > 0 =
        let (_left, right) = fld ! marble in
        move fld right (steps - 1)
    | steps < 0 =
        let (left, _right) = fld ! marble in
        move fld left (steps + 1)
    | otherwise = (marble, fld ! marble)
