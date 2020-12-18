import           Data.List  (findIndex, iterate')
import qualified Data.Map   as M

input :: [Int]
input = reverse [9,6,0,10,18,2,1]

example :: [Int]
example = reverse [0,3,6]

-- Maps from number to how many turns ago it was last said
type NumberLog = M.Map Int Int

-- Third approach:
  -- The NumberLog
  -- A list the the n most recently spoken numbers, it's length
  -- The number last said
type GameState = (NumberLog, ([Int], Int), Int)

recentlySaidMaxLength :: Int
-- About the best for 5000:
-- recentlySaidMaxLength = 400
recentlySaidMaxLength = 800

findFirstOccurrence :: Eq a => a -> [a] -> Maybe Int
findFirstOccurrence a = findIndex (==a)

insertAll :: NumberLog -> [Int] -> NumberLog
insertAll g = go g . reverse . zip [1..]
  where
    go :: NumberLog -> [(Int, Int)] -> NumberLog
    go state [] = state
    go state ((turnSaid, num):xs) =
        go (M.insert num turnSaid state) xs

-- Does not check if the start exceeds the recentlySaidMaxLength
-- Assumes the starting numbers are non empty
startingNumsToGameState :: [Int] -> GameState
startingNumsToGameState [] = error "Empty starting nums"
startingNumsToGameState (x:xs) =
    (insertAll M.empty xs, ([], 0), x)

advanceGame :: GameState -> GameState
advanceGame (numberLog, (recentlySaid, len), lastSaid) =
    case findOccurence lastSaid of
        Nothing -> insertIntoGameState 0
        Just (i, True)  -> insertIntoGameState (i+1)
        Just (i, False) -> insertIntoGameState (i+len)
  where 
    insertIntoGameState :: Int -> GameState
    insertIntoGameState i = if len < recentlySaidMaxLength
        then (numberLog, (lastSaid:recentlySaid, len+1), i) 
        else (updatedNumberLog, ([lastSaid], 1), i)

    -- (i, True ) -> Was found in the list
    -- (i, False) -> Was found in the numberLog
    findOccurence :: Int -> Maybe (Int, Bool)
    findOccurence num =
        case findFirstOccurrence num recentlySaid of
            Nothing -> fmap (\x -> (x, False)) $ M.lookup num numberLog
            Just i -> Just (i, True)

    updatedNumberLog :: NumberLog
    updatedNumberLog =
        insertAll (M.map (+recentlySaidMaxLength) numberLog)
            recentlySaid

advanceGameNTurns :: Int -> GameState -> GameState
advanceGameNTurns n xs = iterate' advanceGame xs !! n

lastSpokenInGameState :: GameState -> Int
lastSpokenInGameState (_, _, x) = x

spokenAtTurn :: [Int] -> Int -> Int
spokenAtTurn start turn = lastSpokenInGameState
    $ advanceGameNTurns
        (turn - length start)
        (startingNumsToGameState start)

main :: IO ()
main = do
    -- print $ spokenAtTurn example 15000
    print $ spokenAtTurn example 30000000
