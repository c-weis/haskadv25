module Day08 (problem1, problem1Test, problem2) where

import Data.Function (on)
import Data.List (partition, sortBy)
import Data.Ord (Down (Down), comparing)
import Data.Set (Set, fromList, intersection, size, unions)
import Utils (splitOnAll)

data Box = Box
  { x :: Int,
    y :: Int,
    z :: Int
  }
  deriving (Show, Eq, Ord)

type BoxPair = (Box, Box)

sqDist :: BoxPair -> Int
sqDist (Box x1 y1 z1, Box x2 y2 z2) = sum . map (^ (2 :: Int)) $ zipWith (-) [x1, y1, z1] [x2, y2, z2]

parseLine :: String -> Box
parseLine l = case map read $ splitOnAll ',' l of
  [x', y', z'] -> Box x' y' z'
  _ -> error $ "Im-pars-ible line: " ++ l

parse :: String -> [Box]
parse = map parseLine . lines

pairs :: [Box] -> [BoxPair]
pairs boxes =
  [ (boxes !! left, boxes !! right)
    | left <- [0 .. maxIdx],
      right <- [0 .. maxIdx],
      left < right
  ]
  where
    maxIdx = length boxes - 1

sortedPairs :: [Box] -> [BoxPair]
sortedPairs = sortBy (compare `on` sqDist) . pairs

type Circuit = Set Box

addLink :: BoxPair -> [Circuit] -> [Circuit]
addLink (b1, b2) circuits = mergeDuplicates (pairSet : circuits)
  where
    pairSet = fromList [b1, b2]
    mergeDuplicates cs =
      let (circuitsWithB, others) = partition (not . null . intersection pairSet) cs
       in unions circuitsWithB : others

buildCircuit :: [BoxPair] -> [Circuit]
buildCircuit = foldl (flip addLink) []

productOfLargest3 :: [Circuit] -> Int
productOfLargest3 = product . take 3 . sortBy (comparing Down) . map size

problem1Template :: Int -> String -> Int
problem1Template connectN = productOfLargest3 . buildCircuit . take connectN . sortedPairs . parse

problem1Test :: String -> String
problem1Test = show . problem1Template 10

problem1 :: String -> String
problem1 = show . problem1Template 1000

finalConnectingPair :: [Box] -> BoxPair
finalConnectingPair boxes = fst . head . dropWhile (not . fullyConnected . snd) . zip links . tail . circuitProgression $ links
  where
    links = sortedPairs boxes
    circuitProgression :: [BoxPair] -> [[Circuit]]
    circuitProgression = scanl (flip addLink) []

    totalCircuitSize = length boxes
    fullyConnected :: [Circuit] -> Bool
    fullyConnected cs = (length cs == 1) && (size (head cs) == totalCircuitSize)

problem2 :: String -> String
problem2 = show . multiplyXs . finalConnectingPair . parse
  where
    multiplyXs :: BoxPair -> Int
    multiplyXs (Box x1 _ _, Box x2 _ _) = x1 * x2
