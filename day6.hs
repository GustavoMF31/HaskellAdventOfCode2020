import Data.List (intersect, nub)
import Data.List.Split (splitOn)

solve1 = sum . map (length . nub . filter (/= '\n')) . splitOn "\n\n"

solve2 =
  sum . map (length . foldl1 intersect . lines) . splitOn "\n\n"

main = do
  contents <- readFile "day6input.txt"
  print $ solve2 contents
