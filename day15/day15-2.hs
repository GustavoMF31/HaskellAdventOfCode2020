import Data.List (iterate')
import qualified Data.Map as M

input :: [Int]
input = reverse [9,6,0,10,18,2,1]

example :: [Int]
example = reverse [0,3,6]

-- Map from number to how many turns ago it was last said
type NumberLog = M.Map Int Int

startNumbersToLog :: [Int] -> (NumberLog, Int)
startNumbersToLog xs = (startNumbersToLogH xs, head xs)

startNumbersToLogH :: [Int] -> NumberLog
startNumbersToLogH [] = M.empty
startNumbersToLogH (x:xs) =
    M.insert x 0 $ M.map (+1) (startNumbersToLogH xs)

newNextNumber :: NumberLog -> Int -> (NumberLog, Int)
newNextNumber numberLog x = case M.lookup x numberLog of
    Nothing -> (M.insert x 1 mapAfterTurn, 0)
    Just val -> (M.insert x 1 mapAfterTurn, val)
  where 
    -- One turn has passed, therefore all nums have been said
    -- one more turn ago
    mapAfterTurn :: M.Map Int Int
    mapAfterTurn = M.map (+1) numberLog

spokenAtTurn :: [Int] -> Int -> Int
spokenAtTurn start turn =
    snd $ (iterate' (uncurry newNextNumber) startingNumberLog) !! (turn - len)
  where
    startingNumberLog :: (NumberLog, Int)
    startingNumberLog = startNumbersToLog start

    len :: Int
    len = length start

main :: IO ()
main = do
    -- According to my profiling, map is always faster

    print $ spokenAtTurn example 15000
