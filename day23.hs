import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import Data.List (iterate', find)

import Debug.Trace

type CupCircle = Seq.Seq Int

input :: CupCircle
input = Seq.fromList $ map (read . pure) "916438275"

biggestCup :: Int
--biggestCup = 9
biggestCup = 1000000

inputToOneMillion :: CupCircle
inputToOneMillion =
    input Seq.>< Seq.fromList [maximum input+1..biggestCup]

shiftCircleBy :: Int -> CupCircle -> CupCircle
shiftCircleBy initialN cups = end Seq.>< start
  where
    (start, end) = Seq.splitAt n cups
    n = initialN `mod` length cups

moveCups :: Seq.Seq Int -> Seq.Seq Int -> Int -> Int -> CupCircle
moveCups movingCups rest shift i =
    shiftCircleBy (-shift) $ insertIntoList movingCups i rest

cupsInMovement :: Int -> CupCircle -> (Seq.Seq Int, Seq.Seq Int)
cupsInMovement startIndex cups = Seq.splitAt 3 $ shiftCircleBy startIndex cups

-- TODO: Manually track the index, so as to never have to search
-- the sequence for the cup
unsafeElemIndex :: Show a => Eq a => Seq.Seq a -> a -> Int
unsafeElemIndex as a = case Seq.elemIndexL a as of
    Nothing -> error $ "The element " ++ show a ++ " not in the list " ++ show as
    Just i -> i

-- crashes with an empty list
getAtIndexModuloLength :: Seq.Seq a -> Int -> a
getAtIndexModuloLength as i =
    fromJust $ Seq.lookup (i `mod` Seq.length as) as

insertIntoList :: Seq.Seq a -> Int -> Seq.Seq a -> Seq.Seq a
insertIntoList fragment i xs = start Seq.>< fragment Seq.>< end
  where
    (start, end) = Seq.splitAt (i `mod` length xs) xs

-- Takes the index of the current cup and the cup arrangement
crabMove :: (Int, CupCircle) -> (Int, CupCircle)
crabMove (currentCupIndex, cups) =
    -- biggestCup is numerically equal to the length of the list of cups
    ((currentCupIndex+1) `mod` biggestCup, newCupCircle)
  where
    newCupCircle =
        moveCups movingCups rest (currentCupIndex+1) (destinationCupIndex+1)
        -- (currentCupIndex+1) instead of 0

    destinationCupIndex :: Int
    destinationCupIndex = unsafeElemIndex rest destinationCup

    destinationCup = case mDestinationCup of
        Nothing -> error "No valid cup for destination"
        Just x -> x

    mDestinationCup :: Maybe Int
    mDestinationCup = find (\cup -> not $ cup `elem` movingCups)
        $ [currentCup-1,currentCup-2..1] ++ [biggestCup,biggestCup-1..1]

    (movingCups, rest) = cupsInMovement (currentCupIndex+1) cups

    currentCup :: Int
    currentCup = case mCurrentCup of
        Nothing -> error $ "Invalid index for current cup: " ++ show currentCupIndex
        Just x -> x

    mCurrentCup :: Maybe Int
    mCurrentCup = Seq.lookup currentCupIndex cups

-- Assumes 1 is one of the cups
labelsAfterOne :: CupCircle -> Seq.Seq Int
labelsAfterOne cups =
    Seq.drop 1 $ shiftCircleBy (unsafeElemIndex cups 1) cups

main :: IO ()
main = print
    $ Seq.take 2
    $ labelsAfterOne $ snd
    $ iterate' crabMove (0, inputToOneMillion) !! 100
