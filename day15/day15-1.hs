import Data.List (iterate')

input :: [Int]
input = reverse [9,6,0,10,18,2,1]

example :: [Int]
example = reverse [0,3,6]

-- Returns the index and the rest of the list
findFirstOccurence :: Eq a => a -> [a] -> Maybe Int
findFirstOccurence _ [] = Nothing
findFirstOccurence a (x:xs)
    | a == x = Just 0
    | otherwise = (+1) <$> findFirstOccurence a xs 

nextNumber :: [Int] -> Int
nextNumber [] = error "No starting numbers"
nextNumber (x:xs) = case findFirstOccurence x xs of
    -- X is brand new
    Nothing -> 0
    Just i -> i+1

advanceGame :: [Int] -> [Int]
advanceGame xs = nextNumber xs : xs

advanceGameNTurns :: Int -> [Int] -> [Int]
advanceGameNTurns n xs = iterate' advanceGame xs !! n

spokenAtTurn :: [Int] -> Int -> Int
spokenAtTurn start turn =
    head $ advanceGameNTurns (turn - length start) start

main :: IO ()
main = do
    print $ spokenAtTurn example 15000
