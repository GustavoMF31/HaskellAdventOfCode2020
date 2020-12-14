type BagColor = String
data BagRule = BagRule { bagColor :: BagColor
                       , contains :: [(Int, BagColor)]
                       } deriving (Show)

-- All of the parsing is partial
-- But it's parsing, so, who cares

-- Works at the level of individual words
parseContainedBags :: [String] -> [(Int, BagColor)]
-- parseContainedBags [] = []
parseContainedBags (c:adj:color:"bags,":rest) =
   (read c, unwords [adj, color]) : parseContainedBags rest
parseContainedBags (c:adj:color:"bag,":rest) =
   (read c, unwords [adj, color]) : parseContainedBags rest
parseContainedBags [c, adj, color, "bags."] =
  [(read c, unwords [adj, color])]
parseContainedBags [c, adj, color, "bag."] =
  [(read c, unwords [adj, color])]
parseContainedBags ["no","other","bags."] = []
parseContainedBags what = error $ "Can't parse: " ++ show what

parseRow :: String -> BagRule
parseRow s =
  let (adj:color:"bags":"contain":containedBags) = words s
  in BagRule (unwords [adj, color]) (parseContainedBags containedBags)

parseAll :: String -> [BagRule]
parseAll = map parseRow . lines

-- Partial,
-- because assumes all the mentioned bags are gonna have a rule defined
bagRuleLookup :: [BagRule] -> BagColor -> BagRule
bagRuleLookup rules bag = head $ filter ((== bag) . bagColor) $ rules

shinyGoldCount :: [BagRule] -> BagColor -> Int
shinyGoldCount db "shiny gold" = 1
shinyGoldCount db bag =
  sum $
  map (\(count, innerBag) -> count * shinyGoldCount db innerBag) $
  contains bagRule

  where bagRule = bagRuleLookup db bag

-- Count of bag colors that have at least one shiny gold in them
solve :: [BagRule] -> Int
solve db = length $ filter validBag db
  where validBag bagRule = bagColor bagRule /= "shiny gold"
                           && shinyGoldCount db (bagColor bagRule) >= 1

totalBagCount :: [BagRule] -> BagColor -> Int
totalBagCount db bag =
  (1 +) $
  sum $
  map (\(count, innerBag) -> count * totalBagCount db innerBag) $
  contains bagRule 

  where bagRule = bagRuleLookup db bag

main = do
  contents <- readFile "day7input.txt"
  -- Returns one more than the answer
  -- (it includes the gold bag itself, while the problem doesn't)
  print $ flip totalBagCount "shiny gold" $ parseAll contents

