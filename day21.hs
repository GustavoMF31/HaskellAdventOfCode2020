import Text.Parsec.String
import Text.Parsec

import Data.Either (rights)
import Data.List (intercalate, sortOn)
import qualified Data.Set as Set
import qualified Data.Map as M

type Allergen = String
type Ingredient = String
type AllergenMap = M.Map Allergen (Set.Set Ingredient)
type SolvedAllergenMap = M.Map Allergen Ingredient

-- States that the food that contains that specific allergen is contained
-- by this set of ingredients
type FoodInfo = (Allergen, [Ingredient])

data Food = Food
  { foodIngredients :: [Ingredient]
  , foodAllergens :: [Allergen]
  } deriving (Show)

pFood :: Parser Food
pFood = do
   ingredients <- many1 letter `sepEndBy1` spaces 
   allergens <- between (char '(') (char ')') pAllergens
   return $ Food ingredients allergens

pAllergens :: Parser [String]
pAllergens = do
    _ <- string "contains"
    spaces
    allergens <- many1 letter `sepBy1` (string ", ")
    return allergens

foodInfosFromFood :: Food -> [FoodInfo]
foodInfosFromFood food = (,) <$> foodAllergens food <*> pure (foodIngredients food)

getSingleElement :: Set.Set a -> Maybe a
getSingleElement set
    | Set.size set == 1 = Just $ Set.elemAt 0 set
    | otherwise = Nothing

-- Checks the map for solved allergens and delets them from other constraints
-- and inserts them in the SolvedAllergenMap
replaceAllSolved :: (SolvedAllergenMap, AllergenMap)
                 -> (SolvedAllergenMap, AllergenMap)
replaceAllSolved (solved, allergenMap) =
    (newSolved, M.map (Set.\\ (Set.fromList $ M.elems newSolved)) allergenMap)
  where
     newSolved = M.foldrWithKey f solved allergenMap

     f :: Allergen -> Set.Set Ingredient -> SolvedAllergenMap -> SolvedAllergenMap
     f allergen ingredients solved = case getSingleElement ingredients of
         Nothing -> solved
         Just ingredient -> M.insert allergen ingredient solved

-- Very hacky way to get all the replacements to be made
replaceAllSequentially :: (SolvedAllergenMap, AllergenMap)
                       -> (SolvedAllergenMap, AllergenMap)
replaceAllSequentially x
    | replaced == x = replaced
    | otherwise = replaceAllSequentially replaced
  where
    replaced = replaceAllSolved x

processInfo :: (SolvedAllergenMap, AllergenMap)
            -> FoodInfo
            -> (SolvedAllergenMap, AllergenMap)
processInfo (solved, allergenMap) (allergen, possibleIngredients)
    -- If this allergen is already solved there is nothing to be done
    | allergen `elem` M.keys solved = (solved, allergenMap)
    | otherwise = case M.lookup allergen allergenMap of
        -- We knew nothing about this allergen
        Nothing -> replaceAllSequentially (solved, M.insert allergen possibleSet allergenMap)
        Just knownPossible ->
            let newPossible = Set.intersection knownPossible possibleSet
            in replaceAllSequentially (solved, M.insert allergen newPossible allergenMap)
      where
        possibleSet :: Set.Set Ingredient 
        possibleSet = Set.difference
            (Set.fromList possibleIngredients)
            (Set.fromList $ M.elems solved)
            
main :: IO ()
main = do
    contents <- readFile "day21input.txt"
    let foods = rights $ map (parse pFood "") $ lines contents
        infos = concatMap foodInfosFromFood foods
        (solved, _) = foldl processInfo (M.empty, M.empty) infos
        ingredientOccurences = concat $ map foodIngredients foods
        contaminedIngredients = map snd $ M.toList solved
        contaminedInOrder = sortOn fst (M.toList solved)

    print $ intercalate "," $ map snd $ contaminedInOrder

    {-
    print solved
    print $ length $
        [x | x <- ingredientOccurences, not (x `elem` contaminedIngredients)]

    print $ foods !! 3
    print $ foodInfosFromFood $ foods !! 3
    -}
