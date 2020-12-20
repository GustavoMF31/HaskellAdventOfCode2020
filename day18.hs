import Text.Parsec
import Text.Parsec.String

import Data.Either (rights)

data Operator
    = Plus
    | Times
    deriving (Show)

data Expr
    = Number Int
    | Op Operator Expr Expr

instance Show Expr where
    show (Number     x) = show x
    show (Op Plus   x y) = '(':show x ++ "+" ++ show y ++ ")"
    show (Op Times x y) = '(':show x ++ "*" ++ show y ++ ")"

parseExpr :: Parser Expr
parseExpr = do
    maybeDigit <- optionMaybe digit

    case maybeDigit of
        Nothing -> do
            lExpr <- between (char '(') (char ')') parseExpr

            maybeOp <- optionMaybe parseOperator
            case maybeOp of
                Nothing -> return lExpr
                Just op -> do
                    rExpr <- parseExpr
                    return $ Op op lExpr rExpr

        Just d -> do
            maybeOp <- optionMaybe parseOperator
            case maybeOp of
                Nothing -> return $ Number (read [d])
                Just op -> do
                    rExpr <- parseExpr
                    return $ Op op (Number $ read [d]) rExpr

-- Consumes the whitespace around it as well
parseOperator :: Parser Operator
parseOperator = do
    spaces
    op <- choice
        [ char '+' >> return Plus
        , char '*' >> return Times
        ]
    spaces

    return op

eval :: Expr -> Int
eval (Number x) = x
eval (Op Plus x y) = eval x + eval y
eval (Op Times x y) = eval x * eval y

transformParens :: String -> String
transformParens = map transformChar
  where
    transformChar :: Char -> Char
    transformChar '(' = ')'
    transformChar ')' = '('
    transformChar c = c

continueExpr :: Expr -> Parser Expr
continueExpr expr = option expr (continueExprFallible expr)

continueExprFallible :: Expr -> Parser Expr
continueExprFallible lExpr = do
    op <- parseOperator
    case op of
        Plus -> do
            rExpr <- grabExpr
            return $ Op Plus lExpr rExpr
        Times -> do
            rExpr <- fullExpr
            return $ Op Times lExpr rExpr


isPlus :: Expr -> Bool
isPlus (Op Plus _ _) = True
isPlus _ = False

-- Eats as much as it can

continueWhilePossible :: Expr -> Parser Expr
continueWhilePossible expr = do
    maybeContinued <- optionMaybe (continueExprFallible expr)
    case maybeContinued of 
        Nothing -> return expr
        Just continued -> continueWhilePossible continued

fullExpr :: Parser Expr
fullExpr = do
    expr <- grabExpr
    continued <- continueWhilePossible expr

    return continued

-- Eats as little as it can
grabExpr :: Parser Expr
grabExpr = do
    maybeDigit <- optionMaybe digit
    case maybeDigit of
        Nothing -> between (char '(') (char ')') fullExpr
        Just d -> return $ Number $ read [d]

main :: IO ()
main = do
    contents <- readFile "day18input.txt"
    -- print $ parse parseExpr "" (lines contents !! 0)
    print
        $ sum
        $ map eval
        $ rights
        $ (map (parse fullExpr "")
        $ lines contents)

