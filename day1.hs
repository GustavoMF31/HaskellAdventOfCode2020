third  (a, b, c) = c
fourth (a, b, c, d) = d

example :: [Int]
example =
  [ 1721
  , 979
  , 366
  , 299
  , 675
  , 1456
  ]

-- Let's solve it the dumb way
solve :: [Int] -> Int
solve xs = x * y
  where (x, y, _) = head $
                    filter ((== 2020) . third) $
                      (\x y -> (x, y, x + y)) <$> xs <*> xs

solve2 :: [Int] -> Int
solve2 xs = x * y * z
  where (x, y, z, _) = head $
                       filter ((== 2020) . fourth) $
                       (\x y z -> (x, y, z, x + y + z)) <$> xs <*> xs <*> xs

main = do
  contents <- readFile "day1input.txt"
  print $ solve2 $ map read $ lines contents
  return ()
