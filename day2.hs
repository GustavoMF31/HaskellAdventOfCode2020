data PasswordPolicy = Policy
    { policyChar :: Char
    , policyMinOccurences :: Int
    , policyMaxOccurences :: Int
    } deriving (Show)

countElem :: Eq a => a -> [a] -> Int
countElem _ [] = 0
countElem a (x:xs)
    | a == x = 1 + countElem a xs
    | otherwise = countElem a xs

validatePassword :: PasswordPolicy -> String -> Bool
validatePassword (Policy c minOccurences maxOccurences) password =
    minOccurences <= count && count <= maxOccurences
  where
    count :: Int
    count = countElem c password 

validatePassword2 :: PasswordPolicy -> String -> Bool
validatePassword2 (Policy c index1 index2) password =
    (password !! (index1 - 1) == c)
    /= (password !! (index2 - 1) == c)

countValid :: (PasswordPolicy -> String -> Bool) -> [(PasswordPolicy, String)] -> Int
countValid validate = length . filter (uncurry validate)

-- Partial, just like read itself
readInt :: String -> (Int, String)
readInt s =
    let taken = takeWhile (\char -> char `elem` ['0'..'9']) s
    in (read taken, drop (length taken) s)

-- Very partial function
parsePolicyAndPassword :: String -> (PasswordPolicy, String)
parsePolicyAndPassword s =
    let (minCount, s') = readInt s
        (maxCount, s'') = readInt $ drop 1 s'
        c = s'' !! 1
        password = drop 4 s''
    in (Policy c minCount maxCount, password)

main :: IO ()
main = do
    contents <- readFile "day2input.txt"
    print $ countValid validatePassword2 $ map parsePolicyAndPassword $ lines contents
    return ()
