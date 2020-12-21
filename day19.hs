import Control.Monad   (void)
import Data.Either     (isRight, rights)
import Data.List.Split (splitOn)

import Text.Parsec
import Text.Parsec.String

import Debug.Trace (trace)

type RuleId = Int

data Rule
    = Character RuleId Char
    | AllOf RuleId [RuleId] 
    | OneOf RuleId [RuleId] [RuleId]
    deriving (Show)

rule :: Parser Rule
rule = do
    ruleId <- read <$> many1 digit
    _ <- char ':'
    spaces

    choice [ quotedChar ruleId, ruleWithListOfIds ruleId]

  where
    quotedChar :: RuleId -> Parser Rule
    quotedChar ruleId = do
        _ <- char '"'
        c <- anyChar
        _ <- char '"'
        return $ Character ruleId c

    ruleWithListOfIds :: RuleId -> Parser Rule
    ruleWithListOfIds ruleId = do
        ids <- (read <$> many1 digit) `sepEndBy1` (space)

        choice
            [ eof >> return (AllOf ruleId ids)
            , char '|' >> spaces >> (read <$> many1 digit) `sepBy1` spaces >>= (return . OneOf ruleId ids)
            ]

getId :: Rule -> RuleId
getId (AllOf x _) = x
getId (OneOf x _ _) = x
getId (Character x _) = x

-- Eric said that the rule id references are always valid,
-- so let's trust him
ruleLookup :: RuleId -> [Rule] -> Rule
ruleLookup x [] = error $ "Eric lied! Look: " ++ show x
ruleLookup ruleId (r:rs)
    | ruleId == getId r = trace ("Lookup: " ++ show ruleId) r
    | otherwise = ruleLookup ruleId rs

matchAllOf :: [Rule] -> [RuleId] -> Parser () -> Parser ()
matchAllOf _ [] cont = cont
matchAllOf rulesDb (x:xs) cont = match rulesDb x (matchAllOf rulesDb xs cont)

-- Let's use parsec itself to solve the problem!
match :: [Rule] -> RuleId -> Parser () -> Parser ()
match ruleDatabase ruleId continuation = case ruleLookup ruleId ruleDatabase of
    AllOf _ rules -> matchAll rules continuation
    OneOf _ rules1 rules2 ->
        (try (matchAll rules1 continuation))
            <|> (matchAll rules2 continuation)
    Character _ c -> char c >> continuation
  where
    matchAll = matchAllOf ruleDatabase

matchExactly :: [Rule] -> RuleId -> Parser ()
matchExactly ruleDb ruleId = match ruleDb ruleId eof

-- main :: IO ()
main = do
    contents <- readFile "day19input2.txt"
    let [stringRules, stringMessages] = splitOn "\n\n" contents
        rules = rights $ map (parse rule "") $ lines stringRules
        messages = lines stringMessages

    -- return $ parse (matchExactly rules 0) ""

    print $ length $
       filter (isRight . parse (matchExactly rules 0) "") messages

    -- print rules
    -- print messages
