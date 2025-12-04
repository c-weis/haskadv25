module Day01 (problem1, problem2) where

parseTurn :: String -> Int
-- TODO: figure out how to panic on bad line
parseTurn ('R' : rest) = read rest
parseTurn ('L' : rest) = -read rest
parseTurn bad_line = error $ "bad line " ++ bad_line ++ ". should start with 'L' or 'R'"

turns :: String -> [Int]
turns = map parseTurn . lines

circleLen :: Int
circleLen = 100

countZeros :: [Int] -> Int
countZeros = length . filter (== 0)

nextClick :: Int -> Int -> Int
nextClick click turn = mod (click + turn) circleLen

finalClicksAtZero :: Int -> [Int] -> Int
finalClicksAtZero start turn_list = countZeros (scanl nextClick start turn_list)

problem1 :: String -> String
problem1 file_content = show $ finalClicksAtZero 50 (turns file_content)

windingIdx :: Int -> Int
windingIdx x = div x circleLen

clicksAtZeroDuringTurn :: Int -> Int -> Int
clicksAtZeroDuringTurn start turn
  | turn >= 0 = abs (windingIdx (start + turn) - windingIdx start)
  | otherwise = clicksAtZeroDuringTurn (-start) (-turn)

startsAndTurns :: Int -> [Int] -> [(Int, Int)]
startsAndTurns initial turn_list = zip (scanl nextClick initial turn_list) turn_list

-- startsAndTurns initial turn_list = (initial, head turn_list) : [(nextClick start previous_turn, next_turn) | ((start, previous_turn), next_turn) <- zip (startsAndTurns initial turn_list) (tail turn_list)]

problem2 :: String -> String
problem2 file_content = show $ sum $ map (uncurry clicksAtZeroDuringTurn) (startsAndTurns 50 (turns file_content))