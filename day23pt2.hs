{-# LANGUAGE TupleSections #-}

import           Data.Function      ((&))
import qualified Data.IntMap.Strict as IntMap
import           Data.List          (find, iterate')
import           Data.Maybe         (fromJust)

-- Everything is coded in the unsafe way that assumes that the keys will
-- always be in the map

type Cup = Int
type CupCircle = IntMap.IntMap Cup

input :: [Cup]
input = [9, 1, 6, 4, 3, 8, 2, 7, 5]

biggestCup :: Cup
biggestCup = 1000000
-- biggestCup = 9

moveCount :: Int
moveCount = 10000000
-- moveCount = 100

allCups :: [Cup]
allCups = input ++ [maximum input +1.. biggestCup]

cupCircleFromList :: [Cup] -> CupCircle
cupCircleFromList [] = IntMap.empty
cupCircleFromList l@(x:xs) = IntMap.fromList $ zip l $ xs ++ [x]

destinationCup :: Cup -> Cup
-- Subtract one extra to make it zero base before the modulus
-- Then add it back to make it one based again
destinationCup n = ((n-2) `mod` biggestCup) + 1

lookupNthSuccesor :: Int -> Cup -> CupCircle -> Cup
lookupNthSuccesor 0 cup _ = cup
lookupNthSuccesor n cup circle =
    lookupNthSuccesor (n-1) (circle IntMap.! cup) circle

lookupFollowing :: Int -> Cup -> CupCircle -> [Cup]
lookupFollowing 0 _ _ = []
lookupFollowing n cup circle = next : lookupFollowing (n-1) next circle
  where
    next :: Cup
    next = circle IntMap.! cup

move :: Cup -> CupCircle -> (Cup, CupCircle)
move 0 circle = error $ "0th cup for circle: " ++ show circle
move currentCup circle = (firstNonMovingCup ,) $ circle
    & IntMap.insert currentCup firstNonMovingCup
    & IntMap.insert destination (head movingCups)
    & IntMap.insert (last movingCups) cupAfterDestination
  where
    cupAfterDestination = case circle IntMap.!? destination of
        Nothing -> error $ "Bad destination: " ++ show destination
        Just x -> x

    firstNonMovingCup = case circle IntMap.!? last movingCups of
        Nothing -> error $ "Bad last moving cup: " ++ show (last movingCups)
        Just x -> x

    -- Safe fromJust due to how find behaves with infinite lists
    destination = fromJust
        $ find (not . (`elem` movingCups))
        -- Exclude the cup itself
        $ drop 1
        $ iterate destinationCup currentCup

    movingCups :: [Cup]
    movingCups = lookupFollowing 3 currentCup circle

main :: IO ()
main = do
    let (_, finalState) = iterate'
            (uncurry move)
            (head allCups, cupCircleFromList allCups) !! moveCount

        afterOne = finalState IntMap.! 1
        afterAfterOne = finalState IntMap.! afterOne

    -- print $ finalState
    print (afterOne, afterAfterOne)
    print $ afterOne * afterAfterOne

