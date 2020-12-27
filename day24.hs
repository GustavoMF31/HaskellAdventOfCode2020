import Text.Parsec
import Text.Parsec.String

import Data.List (iterate')
import Data.Either (rights)
import qualified Data.Set as Set

data HexDirection
    = E
    | SE
    | SW
    | W
    | NW
    | NE
    deriving (Show)

type HexPath = [HexDirection]
type TileMap = Set.Set (Int, Int)

pHexDir :: Parser HexDirection
pHexDir = do
    choice
        [ try $ string "se" >> return SE
        , try $ string "sw" >> return SW
        , try $ string "nw" >> return NW
        , try $ string "ne" >> return NE
        , char 'e'    >> return E
        , char 'w'    >> return W
        ]

pHexPath :: Parser HexPath
pHexPath = do
    directions <- many pHexDir
    eof
    return directions

move :: (Int, Int) -> HexDirection -> (Int, Int)
move (x,y) E  = (x+2,y)
move (x,y) W  = (x-2,y)
move (x,y) SE = (x+1,y-1)
move (x,y) SW = (x-1,y-1)
move (x,y) NE = (x+1,y+1)
move (x,y) NW = (x-1,y+1)

immediatlyAdjacent :: (Int, Int) -> [(Int, Int)]
immediatlyAdjacent t = [move t dir | dir <- [E,W,SE,SW,NE,NW]]

-- Starts from (0, 0)
followPath :: HexPath -> (Int, Int)
followPath = foldl move (0,0)

insertAll :: Ord a => Set.Set a -> [a] -> Set.Set a
insertAll = foldl (flip Set.insert)

tilesToConsiderFlipping :: TileMap -> Set.Set (Int, Int)
tilesToConsiderFlipping s =
    foldl (\set t -> insertAll set (immediatlyAdjacent t)) s s

applyFlipRules :: Int -> Bool -> Bool
applyFlipRules blackAdjacents isBlack
    | isBlack && (blackAdjacents == 0 || blackAdjacents > 2) = True
    | not isBlack && blackAdjacents == 2 = True
    | otherwise = False

flipTile :: TileMap -> (Int, Int) -> TileMap
flipTile tileMap tile
    | Set.member tile tileMap = Set.delete tile tileMap
    | otherwise = Set.insert tile tileMap

flipRequiredTiles :: TileMap -> TileMap
flipRequiredTiles blackTiles =
    foldl flipTile blackTiles
        $ Set.filter (shouldFlip blackTiles)
        $ tilesToConsiderFlipping blackTiles

shouldFlip :: TileMap -> (Int, Int) -> Bool
shouldFlip blackTiles tile = applyFlipRules
    (length
        $ filter id
        $ map (`Set.member` blackTiles) (immediatlyAdjacent tile))

    (Set.member tile blackTiles)

main :: IO ()
main = do
    contents <- readFile "day24input.txt"
    let paths = map (parse pHexPath "") $ lines contents
        endingTiles = map followPath (rights paths)
        blackTiles = foldl flipTile Set.empty endingTiles

    -- sequence_ $ map print paths
    -- sequence_ $ map print endingTiles
    -- print $ length paths
    print $ Set.size $ iterate' flipRequiredTiles blackTiles !! 100

