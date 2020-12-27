import Data.List.Split (splitOn)

type Deck = [Int]
data GameState
    = Active (Deck, Deck)
    -- Holds the deck of the winner
    -- (the only non-empty deck)
    | Won Deck

readInput :: String -> [[Int]]
readInput = map (map read . drop 1 . lines) . splitOn "\n\n"

playRound :: (Deck, Deck) -> GameState
playRound ([], ys) = Won ys
playRound (xs, []) = Won xs
playRound (x:xs, y:ys)
    -- Assumes no equal cards exist
    | x > y = Active (xs ++ [x, y], ys)
    | otherwise = Active (xs, ys ++ [y, x])

-- Returns the winner's deck
play :: (Deck, Deck) -> Deck
play decks = case playRound decks of
    Won winnerDeck -> winnerDeck
    Active decksAfterRound -> play decksAfterRound

data RCResult = P1Won Deck | P2Won Deck deriving (Show)

playRecursiveCombat :: [(Deck, Deck)] -> (Deck, Deck) -> RCResult
-- playRecursiveCombat _ decks | trace (show $ decks) False = undefined
playRecursiveCombat _ (xs, []) = P1Won xs
playRecursiveCombat _ ([], ys) = P2Won ys
playRecursiveCombat history (x:xs, y:ys)
    | (x:xs, y:ys) `elem` history = P1Won (x:xs)
    | length xs >= x && length ys >= y =
        -- Recurse
        case playRecursiveCombat [] (take x xs, take y ys) of
            P1Won _ -> playRecursiveCombat newHistory newDecksForP1Win
            P2Won _ -> playRecursiveCombat newHistory newDecksForP2Win

    | x > y = playRecursiveCombat newHistory newDecksForP1Win
    | otherwise = playRecursiveCombat newHistory newDecksForP2Win
  where
    newHistory :: [(Deck, Deck)]
    newHistory = (x:xs, y:ys) : history

    newDecksForP1Win :: (Deck, Deck)
    newDecksForP1Win = (xs ++ [x, y], ys)

    newDecksForP2Win :: (Deck, Deck)
    newDecksForP2Win = (xs, ys ++ [y, x])

deckScore :: Deck -> Int
deckScore = sum . zipWith (*) [1..] . reverse

deck :: RCResult -> Deck
deck (P1Won d) = d
deck (P2Won d) = d

main :: IO ()
main = do
    contents <- readFile "day22input.txt"
    let [p1, p2] = readInput contents

    -- print $ play (p1, p2)
    print $ deckScore $ deck $ playRecursiveCombat [] (p1, p2)
