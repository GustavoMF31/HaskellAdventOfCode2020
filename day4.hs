import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromJust)
import Text.Read (readMaybe)


type PassportFieldMapping = [(String, String)]

requiredFields :: [String]
requiredFields = [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl" , "pid"] -- No cid

validations :: [String -> Bool]
validations =
    [ isValidByr
    , isValidIyr
    , isValidEyr
    , isValidHgt
    , isValidHcl
    , isValidEcl
    , isValidPid
    ]

-- Assumes, for convenience, that the field is always a valid one
getValidation :: String -> (String -> Bool)
getValidation = fromJust . flip lookup (zip requiredFields validations)

isValid :: PassportFieldMapping -> Bool
isValid mapping = all (isJust . flip lookup mapping) requiredFields

isThereAnd :: (a -> Bool) -> Maybe a -> Bool
isThereAnd = maybe False

isValid2 :: PassportFieldMapping -> Bool
isValid2 mapping = all isValidField requiredFields
  where
    isValidField :: String -> Bool
    isValidField field =
        isThereAnd (getValidation field) (lookup field mapping)

isDigit :: Char -> Bool
isDigit = (`elem` ['0'..'9'])

isHexChar :: Char -> Bool
isHexChar = (`elem` ['0'..'9'] ++ ['a'..'f'])

-- Includes both bounds
validate4DigitInteger :: Int -> Int -> String -> Bool
validate4DigitInteger minVal maxVal string =
    let digits = takeWhile isDigit string
        asInt = read digits
    in (length digits == 4)
       && (drop 4 string == "")
       && minVal <= asInt && asInt <= maxVal

isValidByr :: String -> Bool
isValidByr = validate4DigitInteger 1920 2002

isValidIyr :: String -> Bool
isValidIyr = validate4DigitInteger 2010 2020

isValidEyr :: String -> Bool
isValidEyr = validate4DigitInteger 2020 2030

isValidHgt :: String -> Bool
isValidHgt height =
    isThereAnd isValidHeight (readMaybe number) && hasCorrectSize
  where
    number :: String
    number = takeWhile isDigit height

    suffix :: String
    suffix = take 2 (drop (length number) height)

    hasCorrectSize :: Bool    
    hasCorrectSize = length (drop (length number) height) == 2

    isValidHeight :: Int -> Bool
    isValidHeight heightAsInt = case suffix of
        "cm" -> 150 <= heightAsInt && heightAsInt <= 293
        "in" -> 59  <= heightAsInt && heightAsInt <= 76
        _ -> False

isValidHcl :: String -> Bool
isValidHcl hairColor = case take 1 hairColor of
    "#" -> all isHexChar (take 6 $ drop 1 hairColor)
           && length hairColor == 7
    _   -> False

hairColors :: [String]
hairColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidEcl :: String -> Bool
isValidEcl = (`elem` hairColors)

isValidPid :: String -> Bool
isValidPid pid = case splitAt 9 pid of
    (digits, "") -> all isDigit digits
    _            -> False

-- Very very very unsafe, but very very very convenient
forceIntoTuple :: Show a => [a] -> (a, a)
forceIntoTuple [a, b] = (a, b)
forceIntoTuple what = error $ "Cant force into tuple: " ++ show what

parsePassportFieldMapping :: [String] -> PassportFieldMapping
parsePassportFieldMapping =
    map (forceIntoTuple . splitOn ":") . words . unwords

main :: IO ()
main = do
    contents <- readFile "day4input.txt"
    print $ length $ filter isValid2 $ map (parsePassportFieldMapping . lines) $ splitOn "\n\n" contents
    return ()
