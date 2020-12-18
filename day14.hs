import           Control.Monad (foldM)
import           Data.Char     (isDigit)
import qualified Data.Map      as M
import           Data.Maybe    (fromMaybe)

data Bit = B0 | B1 deriving (Show)

type Bits = [Bit]
type Address = Int
type Mask = [Maybe Bit]

data Instruction
    = MaskSet Mask
    | MemWrite Address Int
    deriving (Show)

-- Can we get away with such an inneficient Memory type?
-- (It was enough for part 1, but not for part 2)
data MemoryState = MemoryState
    { loadedMask :: Mask
    , addressSpace :: M.Map Address Int
    } deriving (Show)

showBits :: Bits -> String
showBits = map showBit
  where
    showBit :: Bit -> Char
    showBit B0 = '0'
    showBit B1 = '1'

toInt :: Bits -> Int
toInt = toIntHelper . reverse

-- Expects the highest bit to come last
toIntHelper :: Bits -> Int
toIntHelper [] = 0
toIntHelper (B0:xs) = 2 * toIntHelper xs
toIntHelper (B1:xs) = 1 + 2 * toIntHelper xs

-- All of the parsing is partial
readMaskBit :: Char -> Maybe Bit
readMaskBit '0' = Just B0
readMaskBit '1' = Just B1
readMaskBit 'X' = Nothing
readMaskBit what = error $ "Cant parse mask bit: " ++ show what

parseInstruction :: String -> Instruction
parseInstruction s = case s !! 1 of
    -- The a comes from mAsk
    'a' -> MaskSet $ map readMaskBit $ drop 7 s
    -- The e comes from mEm
    'e' -> MemWrite
        (read $ takeWhile isDigit $ dropWhile (not . isDigit) s)
        (read $ last $ words s)
    _ -> error $ "Can't parse instruction: " ++ s

-- Puts the most significant digit on the left
toBits :: Int -> Bits
toBits = reverse . toBitsHelper

toBitsHelper :: Int -> Bits
toBitsHelper 0 = []
toBitsHelper x = case x `mod` 2 of
    0 -> B0 : toBitsHelper (x `div` 2)
    1 -> B1 : toBitsHelper (x `div` 2)
    _ -> error "Mod 2 can only return zero or one"

mask :: Bits -> Mask -> Bits
mask = zipWith fromMaybe . flip padWithLeadingZeros 36

allPossibleAddressesFromFloating :: [Maybe Bit] -> [Bits]
allPossibleAddressesFromFloating [] = [[]]
allPossibleAddressesFromFloating (Just B0:xs) =
    map (B0:) (allPossibleAddressesFromFloating xs)
allPossibleAddressesFromFloating (Just B1:xs) =
    map (B1:) (allPossibleAddressesFromFloating xs)
allPossibleAddressesFromFloating (Nothing:xs) =
    let addresses = allPossibleAddressesFromFloating xs
    in map (B0:) addresses ++ map (B1:) addresses

floatingMask :: Bits -> Mask -> [Bits]
floatingMask bits m =
    allPossibleAddressesFromFloating $
        zipWith (flip combineBits) m (padWithLeadingZeros bits 36)
  where
    -- Combines the bits according to the decoder v2
    combineBits :: Bit -> Maybe Bit -> Maybe Bit
    combineBits b (Just B0) = Just b
    combineBits _ (Just B1) = Just B1

    -- Nothing means floating
    combineBits _ Nothing = Nothing

-- Does a raw set (no masking)
setInMemory :: M.Map Address Int -> Address -> Int -> M.Map Address Int
setInMemory memory address int = M.insert address int memory

execute :: MemoryState -> Instruction -> MemoryState
execute memory (MaskSet m) = memory { loadedMask = m }
execute memory (MemWrite address value) =
    memory { addressSpace =
                 setInMemory (addressSpace memory) address maskedVal
           }
  where
    maskedBits :: Bits
    maskedBits = mask (toBits value) (loadedMask memory)

    maskedVal :: Int
    maskedVal = toInt maskedBits

executeV2 :: MemoryState -> Instruction -> MemoryState
executeV2 memory (MaskSet m) = memory { loadedMask = m }
executeV2 memory (MemWrite address value) =
    writeToAll (map toInt addressesToWriteTo) value memory
  where
    addressesToWriteTo :: [Bits]
    addressesToWriteTo =
        floatingMask (toBits address) (loadedMask memory)

writeToAll :: [Address] -> Int -> MemoryState -> MemoryState
writeToAll addresses val memory = memory { addressSpace = newMemory }
  where
    newMemory :: M.Map Address Int
    newMemory = 
      foldl (\keyValuePairs address -> setInMemory keyValuePairs address val) (addressSpace memory) addresses

padWithLeadingZeros :: Bits -> Int -> Bits
padWithLeadingZeros bits desiredSize =
    replicate (desiredSize - length bits) B0 ++ bits

foldlLog :: Show a => Show b => (b -> a -> b) -> b -> [a] -> IO b
foldlLog f = foldM (\acc val -> print acc >> print val >> return (f acc val))

main :: IO ()
main = do
    contents <- readFile "day14input.txt"
    let instructions = map parseInstruction $ lines contents
        initialMemory = MemoryState { loadedMask = [], addressSpace = M.empty}
        finalMemory = foldl executeV2 initialMemory instructions

    -- finalMemory <- foldlLog executeV2 initialMemory instructions

    -- print instructions
    print $ M.foldl (+) 0 $ addressSpace finalMemory

