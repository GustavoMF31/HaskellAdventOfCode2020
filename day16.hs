import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, char, anyChar, digit)
import Text.Parsec (parse, many1, manyTill)

import Data.List.Split (splitOn)
import Data.Either (rights)

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

main :: IO ()
main = do
    contents <- readFile "day16input.txt" 
    let (fields, myTicket, nearbyTickets) = parseInput contents

    print fields
    print myTicket
    print nearbyTickets
