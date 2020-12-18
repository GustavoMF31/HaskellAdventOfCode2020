-- import Control.Monad (foldM)

data CardinalDirection = N | S | E | W deriving (Show)

data Instruction
    = MoveInDirection CardinalDirection Int
    -- Amount of quarter turns
    | Turn Int
    | Move Int
    deriving (Show)

data ShipState = ShipState
    { shipX :: Int
    , shipY :: Int
    , shipFacing :: CardinalDirection
    } deriving (Show)

data ShipWaypointState = ShipWaypointState
    { swShipX :: Int
    , swShipY :: Int
    , swWaypointX :: Int
    , swWaypointY :: Int
    } deriving (Show)

-- Partial!
parseInstruction :: String -> Instruction
parseInstruction ('N':i) = MoveInDirection N $ read i
parseInstruction ('S':i) = MoveInDirection S $ read i
parseInstruction ('E':i) = MoveInDirection E $ read i
parseInstruction ('W':i) = MoveInDirection W $ read i

parseInstruction ('L':i) = Turn $ -(read i `div` 90)
parseInstruction ('R':i) = Turn $ read i `div` 90

parseInstruction ('F':i) = Move $ read i
parseInstruction what = error $ "Can't parse: " ++ what
-- End partial

applyNTimes :: a -> (a -> a) -> Int -> a
applyNTimes a _ 0 = a
applyNTimes a f n = f (applyNTimes a f (n-1))

rotateClockwiseAroundTheOrigin :: Int -> Int -> (Int, Int)
rotateClockwiseAroundTheOrigin  x y  = (y, -x)

turnClockwise :: CardinalDirection -> CardinalDirection
turnClockwise N = E
turnClockwise E = S
turnClockwise S = W
turnClockwise W = N

turn :: CardinalDirection -> Int -> CardinalDirection
turn direction turns = applyNTimes direction turnClockwise (turns `mod` 4)

turnWaypoint :: ShipWaypointState -> Int -> ShipWaypointState
turnWaypoint s turns = applyNTimes s rotateWaypoint (turns `mod` 4)

move :: ShipState -> CardinalDirection -> Int -> ShipState
move s N d = s { shipY = shipY s + d }
move s E d = s { shipX = shipX s + d }
move s W d = s { shipX = shipX s - d }
move s S d = s { shipY = shipY s - d }

swWaypointMove :: ShipWaypointState -> CardinalDirection -> Int -> ShipWaypointState
swWaypointMove s N d = s { swWaypointY = swWaypointY s + d }
swWaypointMove s E d = s { swWaypointX = swWaypointX s + d }
swWaypointMove s W d = s { swWaypointX = swWaypointX s - d }
swWaypointMove s S d = s { swWaypointY = swWaypointY s - d }

rotateWaypoint :: ShipWaypointState -> ShipWaypointState
rotateWaypoint s = s { swWaypointX = newWaypointX, swWaypointY = newWaypointY }
  where
    (newWaypointX, newWaypointY) = rotateClockwiseAroundTheOrigin
         (swWaypointX s) (swWaypointY s)

moveToWaypoint :: ShipWaypointState -> Int -> ShipWaypointState
moveToWaypoint s times =
    s { swShipX = times * swWaypointX s + swShipX s
      , swShipY = times * swWaypointY s + swShipY s
      }

evade :: ShipState -> Instruction -> ShipState
-- Let's use a standart cartesian plane (y gets bigger going up)
evade s (Move distance) = move s (shipFacing s) distance
evade s (MoveInDirection dir distance) = move s dir distance
evade s (Turn turns) = s { shipFacing = turn (shipFacing s) turns }

evade2 :: ShipWaypointState -> Instruction -> ShipWaypointState
evade2 s (Move times) = moveToWaypoint s times
evade2 s (MoveInDirection dir distance) = swWaypointMove s dir distance
evade2 s (Turn turns) = turnWaypoint s turns

initialShip :: ShipState
initialShip = ShipState 0 0 E

initialShip2 :: ShipWaypointState
initialShip2 = ShipWaypointState 0 0 10 1

main :: IO ()
main = do
    input <- readFile "day12input.txt"
    let instructions = map parseInstruction $ lines input
        finalState1 = foldl evade initialShip instructions
        finalState2 = foldl evade2 initialShip2 instructions

    {-
    let f = (\shipState m ->
            print (shipState :: ShipWaypointState)
            >> print (m :: Instruction)
            >> return (evade2 shipState m))

    finalStateM <- foldM f initialShip2 instructions
    -}

    print finalState1
    print $ abs (shipX finalState1) + abs (shipY finalState1)

    print finalState2
    print $ abs (swShipX finalState2) + abs (swShipY finalState2)
