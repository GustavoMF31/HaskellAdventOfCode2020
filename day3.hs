data Square
    = Open
    | Tree
    deriving (Eq)

type Map = [[Square]]

dropCols :: Int -> [[a]] -> [[a]]
dropCols n = map (drop n)

-- partial
readSquare :: Char -> Square
readSquare '#' = Tree
readSquare '.' = Open
readSquare x = error $ "Tried to parse " ++ [x]

countTreesHit :: Int -> Int -> Map -> Int
countTreesHit _ _ [] = 0
countTreesHit _ _ ([]:_) = 0 -- I don't think this should ever happen
countTreesHit slopeX slopeY treesMap@((y:_):_)
    | y == Tree = 1 + rest
    | otherwise = rest
  where
    rest = countTreesHit slopeX slopeY (drop slopeY $ dropCols slopeX treesMap)

slopes :: [(Int, Int)]
slopes =
    [ (1, 1)
    , (3, 1)
    , (5, 1)
    , (7, 1)
    , (1, 2)
    ]

treesProduct :: Map -> Int
treesProduct treeMap =
    product $ map (flip (uncurry countTreesHit) treeMap) slopes

main :: IO ()
main = do
    contents <- readFile "day3input.txt"
    -- print $ countTreesHit 3 1 $ map cycle $ map (map readSquare) $ lines contents
    print $ treesProduct $ map (cycle . map readSquare) $ lines contents
    return ()
