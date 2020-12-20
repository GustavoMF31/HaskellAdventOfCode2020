import Control.Monad (guard)
import Data.List (nub)

type Pos = (Int, Int, Int, Int)
type Dimension = [Pos]

input :: [Pos]
input = 
    [ (3, 0, 0, 0)
    , (6, 0, 0, 0)
    , (0, 1, 0, 0)
    , (3, 1, 0, 0)
    , (7, 1, 0, 0)
    , (5, 2, 0, 0)
    , (6, 2, 0, 0)
    , (7, 2, 0, 0)
    , (0, 3, 0, 0)
    , (1, 3, 0, 0)
    , (6, 3, 0, 0)
    , (7, 3, 0, 0)
    , (6, 4, 0, 0)
    , (7, 4, 0, 0)
    , (1, 6, 0, 0)
    , (0, 7, 0, 0)
    , (1, 7, 0, 0)
    , (5, 7, 0, 0)
    ]

neighbors3D :: Pos -> [Pos]
neighbors3D (x, y, z, w) = do
    deltaX <- deltas
    deltaY <- deltas
    deltaZ <- deltas
    deltaW <- deltas
    
    let moved = (deltaX x, deltaY y, deltaZ z, deltaW w)
    guard $ moved /= (x, y, z, w)

    return moved
  where
    deltas :: [Int -> Int]
    deltas = [(+1), id, \n -> n-1]

willBeAlive :: Dimension -> Pos -> Bool
willBeAlive dim pos = case pos `elem` dim of
    True -> aliveNeighborCount pos dim `elem` [2,3]
    False -> aliveNeighborCount pos dim == 3

aliveNeighborCount :: Pos -> Dimension -> Int
aliveNeighborCount pos dim = length
    $ filter (`elem` dim)
    $ neighbors3D pos

cubesToConsider :: Dimension -> [Pos]
cubesToConsider = nub . concatMap neighbors3D

step :: Dimension -> Dimension
step dim = filter (willBeAlive dim) $ cubesToConsider dim
