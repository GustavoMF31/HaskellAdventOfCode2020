{-# LANGUAGE TupleSections #-}

import Data.List (find)
import Data.Maybe (fromJust)


annotateWithPrevious :: [a] -> [a] -> [([a], a)]
annotateWithPrevious _ [] = []
-- This case for an empty preambe is a bit weird
-- but I think is worth adding to make the function total
annotateWithPrevious [] l = zip (repeat []) l
annotateWithPrevious preamble@(_:as) (x:xs) =
    (preamble, x) : annotateWithPrevious (as ++ [x]) xs

pairings :: [a] -> [(a, a)]
pairings [] = []
pairings (x:xs) = map (x, ) xs ++ pairings xs

isSumOfElements :: [Int] -> Int -> Bool
isSumOfElements xs x = x `elem` fmap (uncurry (+)) (pairings xs)

-- Hardcoded minimum block size of two
findBlockThatSumsTo :: Int -> [Int] -> Maybe [Int]
findBlockThatSumsTo target = go minimumBlockSize
  where
    minimumBlockSize :: Int
    minimumBlockSize = 2

    go :: Int -> [Int] -> Maybe [Int]
    go _ [] = Nothing
    go blockSize l@(_:xs)

        -- We asked for more elements then there were available.
        -- Therefore, the search failed
        | length block < blockSize = Nothing

        -- Reset the blocksize and look for blocks starting at
        -- the next item
        | blockSum > target = go minimumBlockSize xs

        -- Found it!
        | blockSum == target = Just block

        -- Let's add one more item to the block,
        | otherwise = go (blockSize+1) l

      where
        block :: [Int]
        block = take blockSize l

        blockSum :: Int
        blockSum = sum block

main :: IO ()
main = do
    contents <- readFile "day9input.txt"
    let nums = map read $ lines contents
        (preamble, rest) = splitAt 25 nums
        invalidNum =  
            find (not . uncurry isSumOfElements)
            (annotateWithPrevious preamble rest)
    print $ (\xs -> minimum xs + maximum xs) $ fromJust $ findBlockThatSumsTo (snd $ fromJust invalidNum) nums

