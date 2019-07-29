import Data.Bool (bool)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Set (fromList, member)
import Text.Parsec

main :: IO ()
main = do
    input <- readInput
    print $ solve input

data Velocity = Velocity { dx :: Int, dy :: Int } deriving (Show, Eq)
data Position = Position { x :: Int, y :: Int } deriving (Show, Eq, Ord)
data LightPoint = LightPoint { position :: Position, velocity :: Velocity } deriving (Eq)
data LightPoints = LightPoints { time :: Int, points :: [LightPoint] }
instance Show LightPoints where
    show (LightPoints t points) = unlines $ header : (createRow <$> [minY..maxY])
        where
            header = "Message after " ++ show t ++ " seconds"
            minY = minimum $ y . position <$> points
            maxY = maximum $ y . position <$> points
            minX = minimum $ x . position <$> points
            maxX = maximum $ x . position <$> points
            createRow yCoord = toChar yCoord <$> [minX..maxX]
            pointSet = fromList $ position <$> points
            toChar yCoord xCoord = bool '.' '#' $ member (Position xCoord yCoord) pointSet

readInput :: IO LightPoints
readInput = LightPoints 0 . fmap (either (error . show) id . parse parser "input.txt") . lines <$> readFile "input.txt"
    where
        parser = do
            string "position=<"
            xPos <- numberParser
            string ", "
            yPos <- numberParser
            string "> velocity=<"
            deltaX <- numberParser
            string ", "
            deltaY <- numberParser
            string ">"
            return $ LightPoint (Position xPos yPos) (Velocity deltaX deltaY)
        numberParser = do
            text <- (fmap (const "") space <|> string "-") <> many1 digit
            return $ read text

solve :: LightPoints -> LightPoints
solve initialPoints = fromJust $ find isMessage $ scanl movePoints initialPoints $ repeat  ()
    where
        movePoints (LightPoints t pts) () = LightPoints (t + 1) $ movePoint <$> pts
        movePoint (LightPoint pos vel) = LightPoint (Position (x pos + dx vel) (y pos + dy vel)) vel
        isMessage (LightPoints _ pts) = all (hasNeighbor pts) pts
        hasNeighbor pts point = any (areNeighbors point) pts
        areNeighbors p1 p2 = p1 /= p2 && distance p1 p2 `elem` [1, 2]
        distance (LightPoint (Position x1 y1) _) (LightPoint (Position x2 y2) _) =
            abs (x2 - x1) + abs (y2 - y1)
