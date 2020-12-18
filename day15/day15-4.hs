import Data.List (iterate')
import qualified Data.Map as M

input :: [Int]
input = reverse [9,6,0,10,18,2,1]

example :: [Int]
example = reverse [0,3,6]

type NumberLog = M.Map Int Int

-- The log, how many turns have passed
-- and the last said number
type GameState = ((NumberLog, Int), Int)

startNumbersToLog :: [Int] -> GameState
startNumbersToLog [] = error "No starting nums"
startNumbersToLog (x:xs) = ((startNumbersToLogH xs, 0), x)

startNumbersToLogH :: [Int] -> NumberLog
startNumbersToLogH [] = M.empty
startNumbersToLogH (x:xs) =
    M.insert x 1 $ M.map (+1) (startNumbersToLogH xs)

advanceGame :: GameState -> GameState
advanceGame ((numberLog, passedTurns), lastSaid) =
    case M.lookup lastSaid numberLog of
        Nothing -> insertIntoGameState 0
        Just turns -> insertIntoGameState (turns + passedTurns)
  where
    insertIntoGameState :: Int -> GameState
    insertIntoGameState x =
        ((M.insert lastSaid (-passedTurns) numberLog, passedTurns + 1), x)

advanceGameNTurns :: Int -> GameState -> GameState
advanceGameNTurns n xs = iterate' advanceGame xs !! n

lastSpokenInGameState :: GameState -> Int
lastSpokenInGameState (_, x) = x

spokenAtTurn :: [Int] -> Int -> Int
spokenAtTurn start turn = lastSpokenInGameState
    $ advanceGameNTurns
        (turn - length start)
        (startNumbersToLog start)

main :: IO ()
main = print $ spokenAtTurn input 30000000
