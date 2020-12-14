import Data.List (groupBy)

data Direction = Upper | Lower
                 deriving (Eq) 

-- Partial
charToDirection :: Char -> Direction
charToDirection 'L' = Lower
charToDirection 'R' = Upper
charToDirection 'B' = Upper
charToDirection 'F' = Lower

interpretNumerically :: [Direction] -> Int
interpretNumerically = sum . map maybeRaiseTwo . zip [0..] . reverse
  where maybeRaiseTwo (i, dir) = if dir == Upper
                                   then 2 ^ i
                                   else 0

passToRowAndCol :: [Direction] -> (Int, Int)
passToRowAndCol xs = (intVal `div` 8, intVal `mod` 8)
  where intVal = interpretNumerically xs

ids :: String -> [Int]
ids = map (interpretNumerically . map charToDirection) . lines

--mySeat : [Int] -> Int
mySeat ids = groupBy (\a b -> b == a + 1 || a == b + 1) $ filter (not . (`elem` ids)) [0..1023]

main = do
  contents <- readFile "day5input.txt"
  print $ mySeat $ ids contents

