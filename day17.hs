import           Control.Monad       (guard)
import           Data.List           (iterate')
import           Data.Maybe          (fromJust, fromMaybe)
import qualified Data.Vector         as V
import           Data.Vector.Mutable (write)
import           Prelude             hiding (cycle)

data Cube
    = Active
    | Inactive
    deriving (Show, Eq)

type Pos = (Int, Int, Int)

-- The positive direction and the negative direction (0 is always in positive)

data TwoWayVector a = TwoWayVector (V.Vector a) (V.Vector a)
    deriving (Show)

type Row = TwoWayVector Cube
type Slice = TwoWayVector Row
type PocketDimension = TwoWayVector Slice

-- Partial!
parseInitialSlice :: String -> Slice
parseInitialSlice =
    flip TwoWayVector V.empty
    . V.fromList
    . map (flip TwoWayVector V.empty . V.fromList . map readCube)
    . lines
  where
    readCube :: Char -> Cube
    readCube '#' = Active
    readCube '.' = Inactive
    readCube c = error $ "Can't parse cube: " ++ [c]

linearize :: TwoWayVector a -> V.Vector a
linearize (TwoWayVector positives negative) = V.reverse negative V.++ positives

showSlice :: Slice -> String
showSlice slice = unlines $ V.toList $ V.map (map showCube . V.toList . linearize) $ linearize slice
  where
    showCube :: Cube -> Char
    showCube Active = '#'
    showCube Inactive = '.'

third :: (a, b, c) -> c
third (_, _, x) = x

showDimension :: PocketDimension -> String
showDimension dim = unlines $ zipWith (\i s -> "z=" ++ show i ++ "\n" ++ s) [zStart..] $ map showSlice $ V.toList $ linearize dim
  where
    zStart = fst $ third $ getDimensionSize dim

-- Gets at a specific coordinate from vector specifying
-- Positive and negative elements
getAtIndex :: TwoWayVector a -> Int -> Maybe a
getAtIndex (TwoWayVector positives negatives) x
    | x >= 0 = positives V.!? x
    | otherwise = negatives V.!? (-x + 1)

getAtPos :: PocketDimension -> Pos -> Maybe Cube
getAtPos dimension (x, y, z) = do
    slice <- getAtIndex dimension z
    row   <- getAtIndex slice y
    getAtIndex row x

vectorSet :: V.Vector a -> Int -> a -> V.Vector a
vectorSet v i val = V.modify (\s -> write s i val) v

vectorExpandSet :: V.Vector a -> Int -> a -> a -> V.Vector a
vectorExpandSet vect i val filler
    | V.length vect > i = vectorSet vect i val
    | otherwise = vectorSet enlargedVect i val
  where
    enlargedVect = vect V.++ V.replicate ((i - V.length vect) + 1) filler

expandSetAtIndex :: TwoWayVector a -> Int -> a -> a -> TwoWayVector a
expandSetAtIndex (TwoWayVector pos neg) index value filler
    | index >= 0 = TwoWayVector (vectorExpandSet pos index value filler) neg
    | otherwise  = TwoWayVector pos (vectorExpandSet neg negIndex value filler)
  where
    negIndex = -index - 1

setAtSliceIndex :: Slice -> (Int, Int) -> Cube -> Slice
setAtSliceIndex slice (x, y) cube = expandSetAtIndex slice y newRow emptyRow
  where
    oldRow = fromMaybe emptyRow $ getAtIndex slice y
    newRow = expandSetAtIndex oldRow x cube Inactive
    emptyRow = TwoWayVector V.empty V.empty

setAtDimensionIndex :: PocketDimension -> (Int, Int, Int) -> Cube -> PocketDimension
setAtDimensionIndex dimension (x, y, z) cube =
    expandSetAtIndex dimension z newSlice emptySlice
  where
    oldSlice = fromMaybe emptySlice $ getAtIndex dimension z
    newSlice = setAtSliceIndex oldSlice (x, y) cube
    emptySlice = TwoWayVector V.empty V.empty

neighbors3D :: Pos -> [Pos]
neighbors3D (x, y, z) = do
    deltaX <- deltas
    deltaY <- deltas
    deltaZ <- deltas
    
    let moved = (deltaX x, deltaY y, deltaZ z)
    guard $ moved /= (x, y, z)

    return moved
  where
    deltas :: [Int -> Int]
    deltas = [(+1), id, \n -> n-1]

updateCube :: Cube -> Int -> Cube
updateCube Active activeNeighbors
    | activeNeighbors `elem` [2, 3] = Active
    | otherwise = Inactive
updateCube Inactive activeNeighbors
    | activeNeighbors == 3 = Active
    | otherwise = Inactive

getAllNeighbors :: PocketDimension -> Pos -> [Maybe Cube]
getAllNeighbors dimension = map (getAtPos dimension) . neighbors3D

type Range = (Int, Int)

combineRange :: (Ord a, Ord b) => (a, b) -> (a, b) -> (a, b)
combineRange (min1, max1) (min2, max2) = (min min1 min2, max max1 max2)

combineRanges :: (Ord a1, Ord b1, Ord a2, Ord b2)
              => ((a1, b1), (a2, b2))
              -> ((a1, b1), (a2, b2))
              -> ((a1, b1), (a2, b2))
combineRanges (a, b) (c, d) = (combineRange a c, combineRange b d)

getTwoWayVectorSize :: TwoWayVector a -> Range
getTwoWayVectorSize (TwoWayVector pos neg) = (-(V.length neg), V.length pos - 1)

getSliceSize :: Slice -> (Range, Range)
getSliceSize slice@(TwoWayVector pos neg) = (rangeX, getTwoWayVectorSize slice)
  where
    rangeX = V.foldl combineRange (0, 0) $ V.map getTwoWayVectorSize (pos V.++ neg)

getDimensionSize :: PocketDimension -> (Range, Range, Range)
getDimensionSize dimensions@(TwoWayVector pos neg) = (rangeX, rangeY, getTwoWayVectorSize dimensions)
  where
    (rangeX, rangeY) =
        V.foldl combineRanges ((0, 0), (0,0)) $ V.map getSliceSize (pos V.++ neg)

increase3DRange :: (Range, Range, Range) -> (Range, Range, Range)
increase3DRange ((minX, maxX), (minY, maxY), (minZ, maxZ)) =
    ((minX-1, maxX+1), (minY-1, maxY+1), (minZ-1, maxZ+1))

cycle :: PocketDimension -> PocketDimension
cycle dimension = foldl (\dim pos ->
        setAtDimensionIndex dim pos (newCube dimension pos)) dimension
    $ generateAllPositions
    $ increase3DRange
    $ getDimensionSize dimension

  {-
  where
    step :: PocketDimension -> Pos -> PocketDimension
    step dim pos = fromMaybe dim $ do
        nextCubeState <- newCube dimension pos
  -}


generateAllPositions :: (Range, Range, Range) -> [Pos]
generateAllPositions ((minX, maxX), (minY, maxY), (minZ, maxZ)) =
    [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]

newCube :: PocketDimension -> Pos -> Cube
newCube dimension pos =
    updateCube (fromMaybe Inactive $ getAtPos dimension pos)
    $ length
    $ filter ((==) Active . fromMaybe Inactive)
    $ getAllNeighbors dimension pos

initialPocketDimension :: Slice -> PocketDimension
initialPocketDimension initialSlice =
    TwoWayVector (V.singleton initialSlice) V.empty

countActive :: PocketDimension -> Int
countActive = V.sum . V.map (V.sum . V.map (V.length . V.filter ((==) Active) . linearize) . linearize) . linearize

main :: IO ()
main = do
    contents <- readFile "day17example.txt"
    let dimension = initialPocketDimension $ parseInitialSlice contents 
        afterBootCycles = iterate' cycle dimension !! 1
        
    putStrLn $ showDimension $ setAtDimensionIndex dimension (-1, -1, -1) Active

