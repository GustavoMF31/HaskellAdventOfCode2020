import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, char, anyChar, digit)
import Text.Parsec (parse, many1, manyTill)

import Data.List.Split (splitOn)
import Data.List (isInfixOf, deleteBy, sortOn, transpose)
import Data.Either (rights)
import Data.Function (on)
import Data.Bifunctor (second)

parseFieldRule :: Parser FieldRule
parseFieldRule = do
    name <- manyTill anyChar (char ':')
    _ <- char ' '
    range1 <- parseRange 
    _ <- string " or "
    range2 <- parseRange
    return $ FieldRule name range1 range2

parseRange :: Parser Range
parseRange = do
    rangeStart <- read <$> many1 digit
    _ <- char '-'
    rangeEnd  <- read <$> many1 digit
    return $ Range rangeStart rangeEnd

parseInput :: String -> ([FieldRule], Ticket, [Ticket])
parseInput s =
    let [fields, myTicket, tickets] = splitOn "\n\n" s
    in ( rights $ map (parse parseFieldRule "problem") (lines fields)
       , map read $ splitOn "," myTicket
       , map (map read . splitOn ",") $ lines tickets
       )

data Range = Range
    { lowerBound :: Int
    , upperBound :: Int
    } deriving (Show)

data FieldRule = FieldRule
    { frName :: String
    , frRange1 :: Range
    , frRange2 :: Range
    } deriving (Show)

type Ticket = [Int]

isWithin :: Int -> Range -> Bool
isWithin x (Range rangeStart rangeEnd) = rangeStart <= x && x <= rangeEnd

isValidForField :: Int -> FieldRule -> Bool
isValidForField x field =
    x `isWithin` frRange1 field || x `isWithin` frRange2 field

isPossibleValue :: [FieldRule] -> Int -> Bool
isPossibleValue fields value = any (isValidForField value) fields

findImpossibleFields :: [FieldRule] -> Ticket -> [Int]
findImpossibleFields fields = filter (not . isPossibleValue fields)

isFieldValidForColumn :: FieldRule -> [Int] -> Bool
isFieldValidForColumn = all . flip isValidForField

-- The input list at index i is a list of all valid fields for
-- the i-th column
assignFieldsToColumns :: [(Int, [FieldRule])] -> [[(Int, FieldRule)]]
assignFieldsToColumns [] = return []
assignFieldsToColumns ((i, possibleFields):rest) = do
    fieldForIndexI <- possibleFields
    restOfTheAssignment <-
        assignFieldsToColumns (removeField fieldForIndexI rest)

    return $ (i, fieldForIndexI): restOfTheAssignment

  where
    removeField :: FieldRule
                -> [(Int, [FieldRule])]
                -> [(Int, [FieldRule])]
    removeField _ [] = []
    removeField f ((_, possibleFs):xs) =
        (i, deleteBy ((==) `on` frName) f possibleFs)
        : removeField f xs

main :: IO ()
main = do
    contents <- readFile "day16input.txt" 
    let (fields, myTicket, nearbyTickets) = parseInput contents
        validTickets = filter (null . findImpossibleFields fields) nearbyTickets
        cols = transpose validTickets
        -- testField = FieldRule {frName = "departure location", frRange1 = Range {lowerBound = 44, upperBound = 401}, frRange2 = Range {lowerBound = 415 , upperBound = 965}}
        validFields = map (\col -> [ field | field <- fields, isFieldValidForColumn field col ]) cols
        organizedFields = sortOn (length . snd) $ zip [0..] validFields
        assignedFieds = head
            $ map (map (second frName))
            $ assignFieldsToColumns organizedFields

    -- Part one:
    -- print $ sum $ concat $ map (findImpossibleFields fields) nearbyTickets

    print
        $ product
        $ map snd
        $ filter (isInfixOf "departure" . fst)
        $ flip zip myTicket
        $ map snd
        $ sortOn fst assignedFieds
