module Day07 (problem1, problem2) where

riskyElemIndex :: (Eq a) => a -> [a] -> Int
riskyElemIndex x ys = head [idx | (y, idx) <- zip ys [0 ..], x == y]

type SplitterMask = [Bool]

data TachyonManifold = TachyonManifold
  { _startPos :: Int,
    _splitters :: [SplitterMask]
  }

data TachyonState = TachyonState
  { _positions :: [Int],
    splitsSoFar :: Int
  }

combineStrictlyIncreasing :: [Int] -> [Int] -> [Int]
combineStrictlyIncreasing l [] = l
combineStrictlyIncreasing [] r = r
combineStrictlyIncreasing l@(x : xs) r@(y : ys)
  | x < y = x : combineStrictlyIncreasing xs r
  | x > y = y : combineStrictlyIncreasing l ys
  | otherwise = combineStrictlyIncreasing l ys

combineStrictlyIncreasing' :: [[Int]] -> [Int]
combineStrictlyIncreasing' = foldl1 combineStrictlyIncreasing

travel :: TachyonState -> SplitterMask -> TachyonState
travel (TachyonState poss splitsYet) splitterMask = TachyonState newPoss (splitsYet + newSplits)
  where
    splitPoss = filter (splitterMask !!) poss
    nonSplitPoss = filter (not . (splitterMask !!)) poss
    newPoss = combineStrictlyIncreasing' [nonSplitPoss, map (subtract 1) splitPoss, map (+ 1) splitPoss]
    newSplits = length splitPoss

parse :: String -> TachyonManifold
parse s = TachyonManifold start splitterMask
  where
    start = riskyElemIndex 'S' . head . lines $ s
    splitterMask = map (map (== '^')) . tail . lines $ s

runBeam :: TachyonManifold -> TachyonState
runBeam (TachyonManifold pos splitMasks) = foldl travel (TachyonState [pos] 0) splitMasks

problem1 :: String -> String
problem1 = show . splitsSoFar . runBeam . parse

newtype QuantumTachyonState = QTS {worldLinesByPosition :: [Int]} deriving (Show)

travel' :: QuantumTachyonState -> SplitterMask -> QuantumTachyonState
travel' (QTS counts) splitter = QTS [totalWorldLines idx splitMaskAbove | (idx, splitMaskAbove) <- zip [0 ..] tripleSplitMask]
  where
    tripleSplitMask = zip3 (False : init splitter) splitter (tail splitter ++ [False])
    totalWorldLines pos (left, middle, right) =
      (if left then counts !! (pos - 1) else 0)
        + (if not middle then counts !! pos else 0)
        + (if right then counts !! (pos + 1) else 0)

initialQTS :: TachyonManifold -> QuantumTachyonState
initialQTS (TachyonManifold start splitMasks) = QTS [if p == start then 1 else 0 | p <- [0 .. (length (head splitMasks) - 1)]]

runBeam' :: TachyonManifold -> QuantumTachyonState
runBeam' tm@(TachyonManifold _ splitMasks) = foldl travel' (initialQTS tm) splitMasks

problem2 :: String -> String
problem2 = show . sum . worldLinesByPosition . runBeam' . parse
