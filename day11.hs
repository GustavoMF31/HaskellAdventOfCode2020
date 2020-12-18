import Control.Monad (guard)
import Data.Maybe    (mapMaybe)
import qualified Data.Vector as V

-- Remeber to use mapMaybe with the vector

data MapTile
    = Floor
    | EmptySeat
    | OccupiedSeat
    deriving (Eq, Show)

type Vect2D a = V.Vector (V.Vector a)
type Map = Vect2D MapTile

type OccupationCounter = Map -> Int -> Int -> Int
type DirectionFunc = (Int -> Int -> (Int, Int))

-- Partial!
parseMapTile :: Char -> MapTile
parseMapTile '.' = Floor
parseMapTile 'L' = EmptySeat
parseMapTile '#' = OccupiedSeat
parseMapTile c = error $ "Can't parse character: " ++ [c]

parseMapRow :: String -> V.Vector MapTile
parseMapRow = V.fromList . map parseMapTile

parseMap :: String -> Map
parseMap = V.fromList . fmap parseMapRow . lines
-- End partial

infixr 8 ...
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

listPairwise :: [a] -> [(a,a)]
listPairwise [] = []
listPairwise [_] = []
listPairwise (x:y:xs) = (x, y) : listPairwise (y:xs)

mapGet :: Map -> Int -> Int -> Maybe MapTile
mapGet seatMap x y = do
    row <- seatMap V.!? y
    row V.!? x

isOccupied :: MapTile -> Bool
isOccupied OccupiedSeat = True
isOccupied _ = False

neighbors :: Int -> Int -> [(Int, Int)]
neighbors x y = do
    deltaX <- deltas
    deltaY <- deltas
    
    let moved = (deltaX x, deltaY y)
    guard $ moved /= (x, y)

    return moved
  where
    deltas :: [Int -> Int]
    deltas = [(+1), id, \n -> n-1]

countOccupiedNeighbors :: OccupationCounter
countOccupiedNeighbors seatMap =
    length
    . filter isOccupied

    -- Count out of bounds as not occupied
    . mapMaybe (uncurry $ mapGet seatMap)
    ... neighbors

countVisibleOccupiedSeats :: Map -> Int -> Int -> Int
countVisibleOccupiedSeats seatMap x y = length
    $ filter (maybe False id)
    $ map (isVisibleSeatOccupied seatMap x y) directions

directions :: [DirectionFunc]
directions = do
    xMove <- deltas
    yMove <- deltas
    let move x y = (xMove x, yMove y)

    guard $ move 0 0 /= (0,0)
    return move

  where
    deltas :: [Int -> Int]
    deltas = [(+1), id, subtract 1]

-- Returns nothing if there are no visible seats in that direction
isVisibleSeatOccupied :: Map -> Int -> Int -> DirectionFunc -> Maybe Bool
isVisibleSeatOccupied seatMap x y direction = do
    let (tileX, tileY) = direction x y
    tile <- mapGet seatMap tileX tileY
    case tile of
        Floor -> isVisibleSeatOccupied seatMap tileX tileY direction
        EmptySeat    -> Just False
        OccupiedSeat -> Just True
        
transformTile :: Map -> OccupationCounter -> Int -> Int -> Int -> Maybe MapTile
transformTile seatMap counter emptinessThreshold x y = do
    tile <- mapGet seatMap x y
    return $ case tile of
        Floor -> Floor
        EmptySeat -> if counter seatMap x y == 0
            then OccupiedSeat
            else EmptySeat
        OccupiedSeat -> if counter seatMap x y >= emptinessThreshold
            then EmptySeat
            else OccupiedSeat

{-
mapWith2dIndex :: (Int -> Int -> a -> b) -> Vect2D a -> Vect2D b
mapWith2dIndex f = V.imap (\y -> V.imap (\x a -> f x y a))

mapOver2dIndices :: (Int -> Int -> b) -> Vect2D a -> Vect2D b
mapOver2dIndices f = mapWith2dIndex (\x y _ -> f x y)
-}

mapMaybeWith2dIndex :: (Int -> Int -> a -> Maybe b) -> Vect2D a -> Vect2D b
mapMaybeWith2dIndex f = V.imap (V.imapMaybe . flip f)

mapMaybeOver2dIndices :: (Int -> Int -> Maybe b) -> Vect2D a -> Vect2D b
mapMaybeOver2dIndices f = mapMaybeWith2dIndex (\x y _ -> f x y)

update :: OccupationCounter -> Int -> Map -> Map
update counter emptinessThreshold seatMap =
    mapMaybeOver2dIndices (transformTile seatMap counter emptinessThreshold)
    seatMap

update1 :: Map -> Map
update1 = update countOccupiedNeighbors 4

update2 :: Map -> Map
update2 = update countVisibleOccupiedSeats 5

stablePoint :: (Map -> Map) -> Map -> Map
stablePoint updater seatMap = fst $ head $ dropWhile (uncurry (/=)) $ listPairwise $ iterate updater seatMap

countOccupied :: Map -> Int
countOccupied = V.sum . V.map (length . V.filter isOccupied)

showSeat :: MapTile -> Char
showSeat Floor = '.'
showSeat EmptySeat = 'L'
showSeat OccupiedSeat = '#'

showSeatsMap :: Map -> String
showSeatsMap = unlines . V.toList . V.map (V.toList . V.map showSeat)

main :: IO ()
main = do
    input   <- fmap parseMap $ readFile "day11input.txt"
    -- example <- fmap parseMap $ readFile "day11example.txt"
    
    -- putStrLn $ showSeatsMap $ update1 example
    -- print $ countVisibleOccupiedSeats (update1 example) 0 0
    
    print $ countOccupied $ stablePoint update2 $ input

