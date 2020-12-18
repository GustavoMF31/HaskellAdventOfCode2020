import Data.List.Split (splitOn)
import Data.List (sort, sortOn)
import Control.Monad (guard, when)

listPairwise :: [a] -> [(a,a)]
listPairwise [] = []
listPairwise [_] = []
listPairwise (x:y:xs) = (x, y) : listPairwise (y:xs)

addPhone :: [Int] -> [Int]
addPhone xs = maximum xs + 3 : xs

addChargingOutlet :: [Int] -> [Int]
addChargingOutlet = (0:)

type Joltage = Int
newtype Adapter = Adapter { adapterJoltage :: Joltage }
                deriving (Show)

-- Should satisfy the joltage connection condition
type AdapterChain = [Adapter]

arrangements :: [Adapter] -> [AdapterChain]
arrangements adapters = go 0 (sortOn adapterJoltage adapters)
  where
    goalJoltage :: Joltage
    goalJoltage = maximum (map adapterJoltage adapters) + 3

    go :: Joltage -> [Adapter] -> [AdapterChain]
    go joltage []
        | joltage + 3 >= goalJoltage = [[]]
        | otherwise = []
    go joltage (x:xs)
        | joltage + 3 < adapterJoltage x = []
        | otherwise = map (x:) (go (adapterJoltage x) xs)
            ++ go joltage xs

diff adapters = map (uncurry $ flip (-))
    $ listPairwise
    $ 0:sort adapters

count adapters = product
    $ map (length . arrangements . map Adapter)
    $ map (scanl1 (+))
    $ filter ((/= 0) . length)
    $ splitOn [3, 3]
    $ diff adapters

main :: IO ()
main = do
    contents <- readFile "day10input.txt"
    let adapters = map read $ lines contents

        adapterDiff = diff adapters
        sp = splitOn [3, 3] adapterDiff
        filtered = filter ((/=0) . length) sp
        scanned = map (scanl1 (+)) filtered
        lengths = map (length . arrangements . map Adapter) scanned

    {-
    print $ length adapters
    print adapters

    print $ length adapterDiff
    print adapterDiff

    print sp
    print filtered
    print scanned
    print lengths
    -}

    print $ count adapters
    --print $ length $ arrangements $ map Adapter adapters

    {- print $ length (filter (== 1) differences)
          * length (filter (== 3) differences) -}
