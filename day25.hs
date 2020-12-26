import Data.Maybe (fromJust)
import Data.List (find, iterate')

-- Input:
--
-- 8184785
-- 5293040
--
-- Solve 7^n mod 20201227 = 8184785

publicKeyCandidates :: Int -> [Int]
publicKeyCandidates step = iterate' ((`mod` 20201227) . (*step)) 1

loopSize :: Int -> Int
loopSize n = fst $ fromJust
    $ find ((== n) . snd)
    $ zip [0..]
    $ publicKeyCandidates 7

main :: IO ()
main = do
    let cardLoop = loopSize 8184785 --5764801 
        doorLoop = loopSize 5293040 --17807724 

    print $ publicKeyCandidates 8184785 !! doorLoop

