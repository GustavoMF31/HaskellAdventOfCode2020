data Instruction
    = Nop Int
    | Acc Int
    | Jmp Int
    deriving (Show, Eq)

data ProgramExitCondition
    -- Loops holds the value of the accumulator before the first
    -- repeated instrucion
    = Loops Int

    -- Holds the value of the accumulator after running the last
    -- instruction
    | Halts Int

removePositiveSign :: String -> String
removePositiveSign ('+':cs) = cs
removePositiveSign other = other

-- From now on, it's gonna be implied that all the parsing is partial
readInstruction :: String -> Instruction
readInstruction s = case words s of
    ["nop", i] -> Nop $ read $ removePositiveSign i
    ["acc", i] -> Acc $ read $ removePositiveSign i
    ["jmp", i] -> Jmp $ read $ removePositiveSign i
    what       -> error $ "Can't parse: " ++ show what

traceExecution :: [Instruction] -> ProgramExitCondition
traceExecution = go [] 0 0
  where
    go :: [Int] -> Int -> Int -> [Instruction] -> ProgramExitCondition
    -- We are assuming, for convenience of course,
    -- that the program is always well formed
    -- (That is, will never jump out of bounds)
    go linesRan i acc cmds
        | i `elem` linesRan = Loops acc
        | i > length cmds - 1 = Halts acc
        | otherwise = case cmds !! i of
            Nop _      -> go (i:linesRan) (i+1) acc cmds
            Jmp offset -> go (i:linesRan) (i+offset) acc cmds
            Acc delta  -> go (i:linesRan) (i+1) (acc + delta) cmds 

loops :: [Instruction] -> Bool
loops cmds = case traceExecution cmds of
    Loops _ -> True
    Halts _ -> False

-- Returns all the possibilites for swapping one Nop into Jump
-- or one Jmp into Nop
repairProgram :: [Instruction] -> [[Instruction]]
repairProgram [] = []
repairProgram ((Nop i):cmds) =
    (Jmp i:cmds)
    : [Nop i:repairedCmds | repairedCmds <- repairProgram cmds]

repairProgram ((Jmp i):cmds) =
    (Nop i:cmds)
    : [Jmp i:repairedCmds | repairedCmds <- repairProgram cmds]

repairProgram ((Acc i):cmds) =
    [Acc i: repairedCmds
        | repairedCmds <- repairProgram cmds]

-- Assumes there is one program that halts
bruteForceGoodProgram :: [Instruction] -> Int
bruteForceGoodProgram instructions = head $ do
    repairedProgram <- repairProgram instructions
    case traceExecution repairedProgram of
        Halts i -> return i
        Loops _ -> []

main :: IO ()
main = do
    contents <- readFile "day8input.txt"
    let originalProgram = map readInstruction $ lines contents
        -- The problem asserts that the original program loops
        (Loops acc) = traceExecution originalProgram
    
    print $ bruteForceGoodProgram originalProgram

