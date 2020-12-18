import Debug.Trace
import Data.List (minimumBy)
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

secondOfThree (_, x, _) = x

roundUpToMultipleOf :: Int -> Int -> Int
roundUpToMultipleOf i j = ((i `div` j) + 1) * j

solve :: Int -> [Int] -> Int
solve minimumDepartTime buses = bus * wait
  where
    (departTime, bus) =
        minimumBy (compare `on` fst)
        $ map (\bus -> (roundUpToMultipleOf minimumDepartTime bus, bus)) buses
    
    wait = departTime - minimumDepartTime

parseBuses :: String -> [Int]
parseBuses = mapMaybe readMaybe . splitOn ","

parseInput :: String -> (Int, [Int])
parseInput s = (read sMinimumDepartTime, parseBuses sBuses)
  where 
    [sMinimumDepartTime, sBuses] = lines s

             -- numberToModBy, remainder
type CongruenceSystem = [(Int, Int)]

-- Returns the smallest int that satisfies all the congruences
solveCongruences :: CongruenceSystem -> Int
solveCongruences congruences = go (1, 0) congruences
  where
    go :: (Int, Int) -> CongruenceSystem -> Int
    go (_, b) [] = b
    go (m, b) ((p, r):xs) =
        go (m * p, m * (((r - b) * (modularInverse p m)) `mod` p) + b) xs

myGcd :: Int -> Int -> Int
myGcd a b = case a `mod` b of
    0 -> b
    remainder -> myGcd b remainder

-- Pretty much stolen from
-- https://stackoverflow.com/questions/33326716/modular-inverse-in-haskell
extendedEuclidean :: Int -> Int -> (Int, Int, Int)
extendedEuclidean a 0 = (1, 0, a)
extendedEuclidean a b = (x, c-q*x, y)
  where
    q = a `div` b
    r = a `mod` b
    (c, x, y) = extendedEuclidean b r

modularInverse :: Int -> Int -> Int
modularInverse = secondOfThree ... extendedEuclidean

parseCongruences :: String -> [(Int, Int)]
parseCongruences =
    mapMaybe (\(i, int) -> readMaybe int >>= (\v -> return (v, -i))) .  zip [0..] . splitOn ","

main :: IO ()
main = do
    contents <- readFile "day13input.txt" 
    let (minimumDepartTime, buses) = parseInput contents
        congruences = parseCongruences (lines contents !! 1)

    -- print $ solve minimumDepartTime buses
    print congruences
    print $ solveCongruences congruences
