import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import Data.List (iterate', find)

type CupCircle = Seq.Seq Int

-- biggestCup = 9
input :: CupCircle
input = Seq.fromList $ map (read . pure) "916438275"

biggestCup :: Int
biggestCup = 1000000

inputToOneMillion :: CupCircle
inputToOneMillion =
    input Seq.>< Seq.fromList [maximum input+1..biggestCup]

originalIndex :: Int -> Int
originalIndex 9 = 0
originalIndex 1 = 1
originalIndex 6 = 2
originalIndex 4 = 3
originalIndex 3 = 4
originalIndex 8 = 5
originalIndex 2 = 6
originalIndex 7 = 7
originalIndex 5 = 8
originalIndex x = x-1

data IndexChangeTree = Empty | Node Int Int IndexChangeTree Int IndexChangeTree
    deriving (Show)

applyChangeR :: Int -> Int -> IndexChangeTree -> IndexChangeTree
applyChangeR b d Empty = Node b 0 Empty d Empty
applyChangeR b d (Node val lDelta lNode rDelta rNode)
    | b == val = Node val lDelta lNode (rDelta + d) rNode
    | b < val  = Node val lDelta (applyChangeR b d lNode) (rDelta + d) rNode
    | otherwise= Node val lDelta lNode rDelta (applyChangeR b d rNode)

applyChangeL :: Int -> Int -> IndexChangeTree -> IndexChangeTree
applyChangeL b d Empty = Node b d Empty 0 Empty
applyChangeL b d (Node val lDelta lNode rDelta rNode)
    | b == val = Node val (lDelta+d) lNode rDelta rNode
    | b > val  = Node val (lDelta+d) lNode rDelta (applyChangeL b d rNode)
    | otherwise= Node val lDelta (applyChangeL b d lNode) rDelta rNode

-- the rDelta applies to the node as well
-- but the lDelta just applies to the values to the left of the node
getChangeForIndex :: Int -> IndexChangeTree -> Int
getChangeForIndex _ Empty = 0
getChangeForIndex i (Node val lDelta lNode rDelta rNode)
    | i == val = rDelta
    | i > val = rDelta + getChangeForIndex i rNode
    | otherwise = lDelta + getChangeForIndex i lNode

shiftCircleBy :: Int -> CupCircle -> CupCircle
shiftCircleBy initialN cups = end Seq.>< start
  where
    (start, end) = Seq.splitAt n cups
    n = initialN `mod` length cups

-- Returns the taken elements and the new circle
takeFromCircle :: Int -> Int -> CupCircle -> (Seq.Seq Int, CupCircle)
takeFromCircle initialIndex howManyToTake cups =
     Seq.splitAt howManyToTake $ shiftCircleBy i cups
  where
    i = initialIndex `mod` length cups

-- Hangs forever with an empty list
getAtIndexModuloLength :: Seq.Seq a -> Int -> a
getAtIndexModuloLength as i =
    fromJust $ Seq.lookup (i `mod` Seq.length as) as

insertIntoList :: Seq.Seq a -> Int -> Seq.Seq a -> Seq.Seq a
insertIntoList fragment i xs = start Seq.>< fragment Seq.>< end
  where
    (start, end) = Seq.splitAt i xs

-- Takes the label of the current cup and the cup arrangement
crabMove
    :: (IndexChangeTree, Int, CupCircle)
    -> (IndexChangeTree, Int, CupCircle)
crabMove (currentCup, cups) =
    (newCurrentCup, insertIntoList cupsTaken (destinationCupIndex+1) newCircle)
  where
    newCurrentCup :: Int
    newCurrentCup =
        getAtIndexModuloLength newCircle (currentCupNewCircleIndex+1)

    currentCupNewCircleIndex :: Int
    currentCupNewCircleIndex = unsafeElemIndex newCircle currentCup

    destinationCupIndex :: Int
    destinationCupIndex = unsafeElemIndex newCircle destinationCup

    destinationCup :: Int
    destinationCup = fromJust $ find (\cup -> not $ cup `elem` cupsTaken)
        $ [currentCup,currentCup-1..1] ++ [biggestCup,biggestCup-1..0]

    (cupsTaken, newCircle) =
        takeFromCircle (unsafeElemIndex cups currentCup + 1) 3 cups

-- Assumes 1 is one of the cups
labelsAfterOne :: CupCircle -> Seq.Seq Int
labelsAfterOne cups =
    Seq.drop 1 $ shiftCircleBy (unsafeElemIndex cups 1) cups

main :: IO ()
main = print
    $ concatMap show
    $ Seq.take 2
    $ labelsAfterOne $ snd
    $ iterate' crabMove (Seq.index input 0, inputToOneMillion) !! 100
