import Text.Parsec
import Data.Maybe(mapMaybe)
import qualified Data.Map.Strict as Map
import Data.List(delete, find)

main :: IO ()
main = do
    input <- readInput
    putStrLn $ show $ solve1 input
    putStrLn $ show $ solve2 input

data Claim =
    Claim {
        claimId :: Int,
        offsetLeft :: Int,
        offsetTop :: Int,
        width :: Int,
        height :: Int
    }
    deriving Eq

readInput :: IO [Claim]
readInput = fmap (mapMaybe (eitherToMaybe . parseClaim) . lines) $ readFile "input.txt"

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left a) = Nothing
eitherToMaybe (Right b) = Just b

parseClaim :: String -> Either ParseError Claim
parseClaim = parse claimParser "input-line"

claimParser :: Parsec String () Claim
claimParser = do
    char '#'
    claimId <- read <$> many1 digit
    space
    char '@'
    space
    offsetLeft <- read <$> many1 digit
    char ','
    offsetTop <- read <$> many1 digit
    char ':'
    space
    width <- read <$> many1 digit
    char 'x'
    height <- read <$> many1 digit
    eof
    return $ Claim claimId offsetLeft offsetTop width height

solve1 :: [Claim] -> Int
solve1 claims = length $ filter (> 1) $ Map.elems $ claimsPositionsWithCount
    where
        claimsPositionsWithCount = foldl (Map.unionWith (+)) Map.empty $ map claimPositionsWithCount claims
        claimPositionsWithCount = Map.fromList . map (\position -> (position, 1)) . claimPositions

claimPositions :: Claim -> [(Int, Int)]
claimPositions claim = [ (x, y) | x <- [left .. right], y <- [top .. bottom] ]
    where
        (left, right, top, bottom) = rectangle claim

rectangle :: Claim -> (Int, Int, Int, Int)
rectangle claim = (left, right, top, bottom)
    where
        left = offsetLeft claim
        right = left + width claim - 1
        top = offsetTop claim
        bottom = top + height claim - 1

solve2 :: [Claim] -> Maybe Int
solve2 claims = fmap (claimId . fst) $ find (not . uncurry intersectAny) $ [ (x, delete x claims) | x <- claims ]

intersectAny :: Claim -> [Claim] -> Bool
intersectAny claim others = any (intersect claim) others

intersect :: Claim -> Claim -> Bool
intersect a b = minRight >= maxLeft && minBottom >= maxTop
    where
        (aLeft, aRight, aTop, aBottom) = rectangle a
        (bLeft, bRight, bTop, bBottom) = rectangle b
        maxLeft = max aLeft bLeft
        minRight = min aRight bRight
        maxTop = max aTop bTop
        minBottom = min aBottom bBottom
