import Data.Bool (bool)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

main :: IO ()
main = do
    input <- readInput
    print $ solve1 input
    print $ solve2 input

data Direction = DirLeft | DirRight | DirUp | DirDown deriving (Show, Eq, Ord)
data Turn = TurnLeft | GoStraight | TurnRight deriving (Show, Eq, Ord)
data Player = Player { location :: (Int, Int), direction :: Direction, nextTurn :: Turn } deriving (Show, Eq, Ord)
data TrackPart = Horizontal | Vertical | DiagonalUp | DiagonalDown | Junction deriving (Show)

readInput :: IO (Map (Int, Int) TrackPart, Set Player)
readInput = snd . foldl parseLine (0, (Map.empty, Set.empty)) . lines <$> readFile "input.txt"
    where
        parseLine (row, (track, players)) line =
            let (_, track', players') = foldl parseCell ((row, 0), track, players) line in
            (row + 1, (track', players'))
        parseCell ((row, col), track, players) ' ' = ((row, col + 1), track, players)
        parseCell ((row, col), track, players) '-' = ((row, col + 1), Map.insert (row, col) Horizontal track, players)
        parseCell ((row, col), track, players) '|' = ((row, col + 1), Map.insert (row, col) Vertical track, players)
        parseCell ((row, col), track, players) '/' = ((row, col + 1), Map.insert (row, col) DiagonalUp track, players)
        parseCell ((row, col), track, players) '\\' = ((row, col + 1), Map.insert (row, col) DiagonalDown track, players)
        parseCell ((row, col), track, players) '+' = ((row, col + 1), Map.insert (row, col) Junction track, players)
        parseCell ((row, col), track, players) '>' = ((row, col + 1), Map.insert (row, col) Horizontal track, Set.insert (Player (row, col) DirRight TurnLeft) players)
        parseCell ((row, col), track, players) '<' = ((row, col + 1), Map.insert (row, col) Horizontal track, Set.insert (Player (row, col) DirLeft TurnLeft) players)
        parseCell ((row, col), track, players) '^' = ((row, col + 1), Map.insert (row, col) Vertical track, Set.insert (Player (row, col) DirUp TurnLeft) players)
        parseCell ((row, col), track, players) 'v' = ((row, col + 1), Map.insert (row, col) Vertical track, Set.insert (Player (row, col) DirDown TurnLeft) players)
        parseCell _ x = error $ "Invalid track element: '" ++ [x] ++  "'"

solve1 :: (Map (Int, Int) TrackPart, Set Player) -> (Int, Int)
solve1 (track, initialPlayers) = swap $ run (Nothing, initialPlayers) $ Set.toList initialPlayers
    where
        run (Just loc, _) _ = loc
        run (Nothing, players) [] = run (Nothing, players) $ Set.toList players
        run (Nothing, players) (player:xs) =
            let player' = movePlayer (track ! location player) player
                players' = Set.delete player players
                crashLocation = findCrash player' players'
                players'' = Set.insert player' players'
            in
                run (crashLocation, players'') xs

solve2 :: (Map (Int, Int) TrackPart, Set Player) -> (Int, Int)
solve2 (track, initialPlayers) = swap $ run initialPlayers $ Set.toList initialPlayers
    where
        run players []
            | Set.null players = error "No players left."
            | [player] <- Set.toList players = location player
            | otherwise = run players $ Set.toList players
        run players (player:xs) =
            let player' = movePlayer (track ! location player) player
                players' = Set.delete player players
                crashLocation = findCrash player' players'
                players'' = Set.filter ((/=) crashLocation . Just . location) $ Set.insert player' players'
                xs' = filter ((/=) crashLocation . Just . location) xs
            in
                run players'' xs'

movePlayerInDirection :: Player -> Player
movePlayerInDirection player@(Player (row, col) DirLeft _) = player { location = (row, col - 1) }
movePlayerInDirection player@(Player (row, col) DirRight _) = player { location = (row, col + 1) }
movePlayerInDirection player@(Player (row, col) DirUp _) = player { location = (row - 1, col) }
movePlayerInDirection player@(Player (row, col) DirDown _) = player { location = (row + 1, col) }

movePlayer :: TrackPart -> Player -> Player
movePlayer Horizontal player@(Player _ DirLeft _) = movePlayerInDirection player
movePlayer Horizontal player@(Player _ DirRight _) = movePlayerInDirection player
movePlayer Vertical player@(Player _ DirUp _) = movePlayerInDirection player
movePlayer Vertical player@(Player _ DirDown _) = movePlayerInDirection player
movePlayer DiagonalUp player@(Player _ DirLeft _) = movePlayerInDirection $ player { direction = DirDown }
movePlayer DiagonalUp player@(Player _ DirRight _) = movePlayerInDirection $ player { direction = DirUp }
movePlayer DiagonalUp player@(Player _ DirUp _) = movePlayerInDirection $ player { direction = DirRight }
movePlayer DiagonalUp player@(Player _ DirDown _) = movePlayerInDirection $ player { direction = DirLeft }
movePlayer DiagonalDown player@(Player _ DirLeft _) = movePlayerInDirection $ player { direction = DirUp }
movePlayer DiagonalDown player@(Player _ DirRight _) = movePlayerInDirection $ player { direction = DirDown }
movePlayer DiagonalDown player@(Player _ DirUp _) = movePlayerInDirection $ player { direction = DirLeft }
movePlayer DiagonalDown player@(Player _ DirDown _) = movePlayerInDirection $ player { direction = DirRight }
movePlayer Junction player@(Player _ dir turn) = movePlayerInDirection $ player { direction = makeTurn dir turn, nextTurn = getNextTurn turn }
movePlayer trackPart player = error $ "Invalid player state. Track part: " ++ show trackPart ++ ", Player: " ++ show player

makeTurn :: Direction -> Turn -> Direction
makeTurn DirLeft TurnLeft = DirDown
makeTurn DirLeft TurnRight = DirUp
makeTurn DirRight TurnLeft = DirUp
makeTurn DirRight TurnRight = DirDown
makeTurn DirUp TurnLeft = DirLeft
makeTurn DirUp TurnRight = DirRight
makeTurn DirDown TurnLeft = DirRight
makeTurn DirDown TurnRight = DirLeft
makeTurn dir GoStraight = dir

getNextTurn :: Turn -> Turn
getNextTurn TurnLeft = GoStraight
getNextTurn GoStraight = TurnRight
getNextTurn TurnRight = TurnLeft

findCrash :: Player -> Set Player -> Maybe (Int, Int)
findCrash player players =
    bool Nothing (Just $ location player) $ Set.member (location player) (Set.map location players)
