third :: (a, b, c) -> c
third  (_, _, c) = c

fourth :: (a, b, c, d) -> d
fourth (_, _, _, d) = d

logFunction2 :: (a -> b -> c) -> a -> b -> (a, b, c)
logFunction2 f a b = (a, b, f a b)

logFunction3 :: (a -> b -> c -> d) -> a -> b -> c -> (a, b, c, d)
logFunction3 f a b c = (a, b, c, f a b c)

-- The blackbird combinator:
-- Composes a function of one argument and a function of two arguments
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

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
  where
    (x, y, _) = head
      $ filter ((== 2020) . third)
      $ logFunction2 (+)
      <$> xs <*> xs

solve2 :: [Int] -> Int
solve2 xs = x * y * z
  where
    (x, y, z, _) = head
      $ filter ((== 2020) . fourth)
      $ logFunction3 ((+) ... (+))
      <$> xs <*> xs <*> xs

main :: IO ()
main = do
    contents <- readFile "day1input.txt"
    print $ solve2 $ map read $ lines contents
    return ()
