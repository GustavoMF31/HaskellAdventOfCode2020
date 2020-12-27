import qualified Control.Monad.State.Lazy as State
import qualified Data.IntMap              as IntMap
import           Data.List                (sort)
import           Data.Maybe               (fromMaybe, listToMaybe)

type Adapter = Int

infixr 8 ...
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

-- Naive solution
connections1 :: [Adapter] -> Adapter -> Int
connections1 _ adapter | adapter < 0 = 0
connections1 _ 0 = 1
connections1 adapters adapter | adapter `notElem` adapters = 0
connections1 adapters adapter =
    sum $ map (connections1 adapters . (+ adapter)) [-1, -2, -3]

-- Memoized:
connections2 :: [Adapter] -> Adapter -> Int
connections2 adapters = snd . go IntMap.empty
  where
    -- Returns the count and the new memo
    go :: IntMap.IntMap Int -> Adapter -> (IntMap.IntMap Int, Int)
    go memo goalAdapter | goalAdapter < 0 = (memo, 0)
    go memo 0 = (memo, 1)
    go memo goalAdapter = case IntMap.lookup goalAdapter memo of
        Just val -> (memo, val)
        Nothing -> if goalAdapter `notElem` adapters then (memo, 0) else 
            let (memo2, prior) = go memo (goalAdapter-1)
                (memo3, beforePrior) = go memo2 (goalAdapter-2)
                (memo4, beforeBeforePrior) = go memo3 (goalAdapter-3)
                count = prior + beforePrior + beforeBeforePrior
            in (IntMap.insert goalAdapter count memo4, count)

-- Let's write it all again, but in the state monad.
-- This makes working with the memoization much easier
connections3 :: [Adapter] -> Adapter -> Int
connections3 adapters adapter = State.evalState (go adapter) IntMap.empty
  where
    go :: Adapter -> State.State (IntMap.IntMap Int) Int
    go 0 = return 1
    go goalAdapter
        | goalAdapter < 0 || goalAdapter `notElem` adapters = return 0
    go goalAdapter = do
        memo <- State.get
        case IntMap.lookup goalAdapter memo of
            Just val -> return val
            Nothing -> do
                -- Now that ugly threading of the memo is gone
                -- (well, not gone, but done for us by the state monad)
                prior             <- go (goalAdapter-1)
                beforePrior       <- go (goalAdapter-2)
                beforeBeforePrior <- go (goalAdapter-3)
                let count = prior + beforePrior + beforeBeforePrior
                State.modify (IntMap.insert goalAdapter count)
                return count

-- Let's do it bottom up as well
connections4 :: [Adapter] -> Adapter -> Int
connections4 = fromMaybe 0 . listToMaybe ... generateConnections

generateConnections :: [Adapter] -> Adapter -> [Int]
generateConnections _ adapter | adapter <= 0 = []
generateConnections _ 1 = [1]
generateConnections adapters goalAdapter
    | goalAdapter `notElem` adapters = 0:previousConnections
    | otherwise = case previousConnections of
        [] -> [1]
        [x] -> [x+1, x]
        [x, y] -> [x+y+1, x, y]
        (x:y:z:xs) -> x+y+z :x:y:z:xs

  where
    previousConnections :: [Int]
    previousConnections = generateConnections adapters (goalAdapter-1) 

main :: IO ()
main = do
    contents <- readFile "day10input.txt"
    let adapters = sort $ map (read :: String -> Int) $ lines contents

    -- Way too slow
    -- print $ connections1 adapters 180

    print $ connections2 adapters 180
    print $ connections3 adapters 180
    print $ connections4 adapters 180
