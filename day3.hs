data Square = Open | Tree
              deriving (Eq)

type Map = [[Square]]

dropCols :: Int -> [[a]] -> [[a]]
dropCols n [] = []
dropCols n (x:xs) = drop n x : dropCols n xs

-- partial
readSquare :: Char -> Square
readSquare '#' = Tree
readSquare '.' = Open
readSquare x = error $ "Tried to parse " ++ [x]

countTreesHit :: Int -> Int -> Map -> Int
countTreesHit slopeX slopeY [] = 0
countTreesHit slopeX slopeY ([]:xs) = 0 -- I don't think this should ever happen
countTreesHit slopeX slopeY map@((y:ys):xs) = if y == Tree then 1 + rest else rest
  where rest = countTreesHit slopeX slopeY (drop slopeY $ dropCols slopeX map)

slopes = [ (1, 1)
         , (3, 1)
         , (5, 1)
         , (7, 1)
         , (1, 2)
         ]

treesProduct treeMap =
  product $ map (flip (uncurry countTreesHit) treeMap) slopes

main = do
  contents <- readFile "day3input.txt"
  -- putStrLn $ show $ countTreesHit 3 1 $ map cycle $ map (map readSquare) $ lines contents
  putStrLn $ show $ treesProduct $ map cycle $ map (map readSquare) $ lines contents
  return ()
