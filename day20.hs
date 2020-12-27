import Prelude hiding (Either(..))

-- import qualified Data.Map as M
import Safe (headMay, atMay)
import Data.List (intercalate, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe)
import Data.Function (on)
import Control.Monad (guard)

-- import Debug.Trace (trace)

data Pixel = Black | White deriving (Eq)
data Side = Top | Right | Bottom | Left
type TileId = Int

-- Should always be 10x10
type Tile = (TileId, Image)
type Image = [[Pixel]]

-- Tiles that can go above, to the right, down and ti the left
-- type JigsawConstraints = M.Map TileId [TileId]

instance Read Pixel where
    readsPrec _ ('#':xs) = [(Black,xs)]
    readsPrec _ ('.':xs) = [(White,xs)]
    readsPrec _ s = error $ "No Pixel parse for: " ++ [head s]

instance Show Pixel where
    show Black = "#"
    show White = "."

mirror :: Image -> Image
mirror = map reverse

getSide :: Side -> Image -> [Pixel]
getSide Top = head
getSide Right = last . mirror . transpose 
getSide Bottom = last
getSide Left = head . mirror . transpose

opposite :: Side -> Side
opposite Top = Bottom
opposite Right = Left
opposite Bottom = Top
opposite Left = Right

fits :: Side -> Image -> Image -> Bool
fits side1 img1 img2 =
    getSide side1 img1 == getSide (opposite side1) img2

tileFits :: Side -> Tile -> Tile -> Bool
tileFits side = fits side `on` snd

rotateR :: Image -> Image
rotateR = mirror . transpose

rotate :: Int -> Image -> Image
rotate n img = iterate rotateR img !! n

select :: [a] -> [(a, [a])]
select [] = []
select (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- select xs]

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

rotateAndFlip :: Image -> [Image]
rotateAndFlip img = do
    orientation <- [0..3]
    shouldMirror <- [True, False]
    if shouldMirror
        then return $ mirror $ rotate orientation img
        else return $ rotate orientation img

rotateAndFlipTile :: Tile -> [Tile]
rotateAndFlipTile (tileId, img) = zip (repeat tileId) $ rotateAndFlip img

-- Straightforward bruteforcy approach
assembleImage :: Int -> [Tile] -> [[[Tile]]]
assembleImage sideLength = go [] []
  where
    go :: [[Tile]] -> [Tile] -> [Tile] -> [[[Tile]]]
--    go allP current unused | trace (show $ (map $ map fst) (current : allP)) False = undefined
    go allPreviousRows currentRow [] = [currentRow : allPreviousRows]
    go allPreviousRows currentRow unusedTiles
        | length currentRow == sideLength =
              go (currentRow:allPreviousRows) [] unusedTiles

        | otherwise = do
            (nextTile, newUnusedTiles) <- select unusedTiles
            rotatedTile <- rotateAndFlipTile nextTile

            -- Must fit the left tile
            guard
                $ fromMaybe True
                $ flip (tileFits Left) rotatedTile
                <$> headMay currentRow

            -- Must fit the tile above
            guard
                $ fromMaybe True
                $ flip (tileFits Top) rotatedTile
                <$> fmap fromJust (atMay
                <$> headMay allPreviousRows
                <*> pure (sideLength - length currentRow - 1))

            go allPreviousRows (rotatedTile:currentRow) newUnusedTiles

readTile :: [String] -> Tile
readTile s = (readId $ head s, map readPixels (tail s))
  where
    readId :: String -> Int
    readId = read . tail . init . dropWhile (/= ' ')

    readPixels :: String -> [Pixel]
    readPixels = map (read . pure)

cornerProduct :: [[Tile]] -> Int
cornerProduct tiles = product $ map ($ tileIds) corners
  where
    tileIds :: [[Int]]
    tileIds = map (map fst) tiles

    corners :: [[[Int]] -> Int]
    corners = [head . head, head . last, last . head, last . last]

removeBorder :: Image -> Image
removeBorder =
    rotate 3 . reverse . drop 1 . reverse . drop 1 . rotate 1
    . reverse . drop 1 . reverse . drop 1

showImg :: Image -> String
showImg = unlines . map (concatMap show)

formImage :: [[Tile]] -> Image
formImage tiles2dList = joinImages tilesWithoutBorder
  where
    tilesWithoutBorder :: [[Image]]
    tilesWithoutBorder = map (map $ removeBorder . snd) tiles2dList

joinImages :: [[Image]] -> Image
joinImages images = concat listOfListsOfPixelRows
  where
    listOfListsOfPixelRows :: [[[Pixel]]]
    listOfListsOfPixelRows =
        map (foldl (zipWith (++)) $ repeat []) images

readImage :: String -> Image
readImage = map (map (read . pure)) . lines

monster :: Image
monster = readImage $ intercalate "\n" [
    "..................#.",
    "#....##....##....###",
    ".#..#..#..#..#..#..."
    ]

-- Matches according to the monster finding rules
match :: Pixel -> Pixel -> Bool
match    Black    Black =  True
match    _        White =  True
match    _        _     =  False

-- Checks the top left corner of the image for a sea monster
matchMonster :: Image -> Bool
matchMonster img
    | imgX < 20 || imgY < 3 = False
    | otherwise = all and $ (zipWith . zipWith) match img monster
  where
    imgY :: Int
    imgY = length img

    imgX :: Int
    imgX = maybe 0 length $ headMay img

imageSections :: Image -> [Image]
imageSections img = do
    dropX <- [0..imgX-1]
    dropY <- [0..imgY-1]

    return $ map (drop dropX) (drop dropY img)

  where
    imgY :: Int
    imgY = length img

    imgX :: Int
    imgX = maybe 0 length $ headMay img

countMonsters :: Image -> Int
countMonsters = length . filter matchMonster . imageSections

-- The number of black pixels minus the number of monster parts
-- Assumes the monsters don't ever overlap
waves :: Image -> Int
waves img = sum (map (length . filter (== Black)) img) - monsterParts
  where
    partsPerMonster :: Int
    partsPerMonster = 15

    monsterParts :: Int
    monsterParts = countMonsters img * partsPerMonster

main :: IO ()
main = do
    contents <- readFile "day20input.txt"
    let tiles = map (readTile . lines) $ splitOn "\n\n" contents
        imagePossibilities = assembleImage 12 tiles

    -- print $ map (map fst) $ imagePossibilities !! 2
    -- putStrLn $ showImg $ joinImages $ map (map snd) $ imagePossibilities !! 2
    -- putStrLn $ showImg $ formImage $ imagePossibilities !! 2

    writeFile "day20formedImage.txt"
        $ showImg
        $ formImage
        $ imagePossibilities !! 2

    img <- readImage <$> readFile "day20formedImage.txt"
    print $ map waves $ rotateAndFlip img
