import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Text.Read (readMaybe)

type PassportFieldMapping = [(String, String)]

isValid :: PassportFieldMapping -> Bool
isValid mapping = 
  all (isJust .  (flip lookup) mapping)
    [ "byr"
    , "iyr"
    , "eyr"
    , "hgt"
    , "hcl"
    , "ecl"
    , "pid"
    -- , "cid"
    ]

isThereAnd :: (a -> Bool) -> Maybe a -> Bool
isThereAnd f maybe =
  case maybe of
    Nothing -> False
    Just x -> f x

isValid2 :: PassportFieldMapping -> Bool
isValid2 mapping = 
  all (== True)
    [ isThereAnd isValidByr $ lookup "byr" mapping
    , isThereAnd isValidIyr $ lookup "iyr" mapping
    , isThereAnd isValidEyr $ lookup "eyr" mapping
    , isThereAnd isValidHgt $ lookup "hgt" mapping
    , isThereAnd isValidHcl $ lookup "hcl" mapping
    , isThereAnd isValidEcl $ lookup "ecl" mapping
    , isThereAnd isValidPid $ lookup "pid" mapping
    -- , "cid"
    ]

isDigit = (`elem` ['0'..'9'])

isValidByr :: String -> Bool
isValidByr byr = 
  let digits = takeWhile isDigit byr
      asInt = read digits
  in (length digits == 4) && (drop 4 byr == "") &&
       1920 <= asInt && asInt <= 2002 

isValidIyr :: String -> Bool
isValidIyr iyr = 
  let digits = takeWhile isDigit iyr
      asInt = read digits
  in (length digits == 4) && (drop 4 iyr == "") &&
       2010 <= asInt && asInt <= 2020 

isValidEyr :: String -> Bool
isValidEyr eyr = 
  let digits = takeWhile isDigit eyr
      asInt = read digits
  in (length digits == 4) && (drop 4 eyr == "") &&
       2020 <= asInt && asInt <= 2030 

isValidHgt :: String -> Bool
isValidHgt height =
  let number = takeWhile isDigit height
  in case readMaybe number of
       Nothing -> False
       Just heightAsInt ->
         let suffix = take 2 (drop (length number) height)
         in (length (drop (length number) height) == 2) &&
             case suffix of
                  "cm" -> 150 <= heightAsInt && heightAsInt <= 293
                  "in" -> 59 <= heightAsInt && heightAsInt <= 76
                  _ -> False

validHexChar = (`elem` ['0'..'9'] ++ ['a'..'f'])

isValidHcl :: String -> Bool
isValidHcl hairColor =
  case take 1 hairColor of
    "#" -> (all validHexChar $ take 6 $ drop 1 hairColor)
           && length hairColor == 7
    _ -> False

isValidEcl = (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

isValidPid pid =
  case (take 9 pid, drop 9 pid) of
    (digits, "") -> all isDigit digits
    _ -> False

-- Very very very unsafe, but very very very convenient
forceIntoTuple :: Show a => [a] -> (a, a)
forceIntoTuple [a, b] = (a, b)
forceIntoTuple what = error $ "Cant force into tuple: " ++ show what

parsePassportFieldMapping :: [String] -> PassportFieldMapping
parsePassportFieldMapping = map (forceIntoTuple . splitOn ":") . words . unwords

main = do
  contents <- readFile "day4input.txt"
  putStrLn $ show $ length $ filter isValid2 $ map parsePassportFieldMapping $ map lines $ splitOn "\n\n" $ contents
  return ()
