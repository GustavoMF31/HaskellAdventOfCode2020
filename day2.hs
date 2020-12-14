data PasswordPolicy = Policy { char :: Char
                             , minOccurences :: Int
                             , maxOccurences :: Int
                             } deriving (Show)

countElem :: Eq a => a -> [a] -> Int
countElem a [] = 0
countElem a (x:xs) = if a == x
                       then 1 + countElem a xs
                       else countElem a xs

validatePassword :: PasswordPolicy -> String -> Bool
validatePassword (Policy c min max) password
  = min <= count && count <= max
  where count = countElem c password 

validatePassword2 :: PasswordPolicy -> String -> Bool
validatePassword2 (Policy c index1 index2) password
  = (password !! (index1 - 1) == c)
    /= (password !! (index2 - 1) == c)

countValid :: (PasswordPolicy -> String -> Bool) -> [(PasswordPolicy, String)] -> Int
countValid validate = length . filter (uncurry validate)

-- Partial, just like read itself
readInt :: String -> (Int, String)
readInt s = let taken = takeWhile (\char -> char `elem` ['0'..'9']) s
            in (read taken, drop (length taken) s)

-- Very partial function
parsePolicyAndPassword :: String -> (PasswordPolicy, String)
parsePolicyAndPassword s
  = let (min, s') = readInt s
        (max, s'') = readInt $ drop 1 s'
        c = head $ drop 1 s''
        password = drop 4 s''
    in (Policy c min max, password)

main = do
  contents <- readFile "day2input.txt"
  putStrLn $ show $ countValid validatePassword2 $ map parsePolicyAndPassword $ lines contents
  return ()
