module Day10 (problem1, problem2) where

import Data.Bits (xor)
import Data.Function (on)
import Data.List (foldl', iterate', nub, nubBy)
import Data.Map (Map, empty, insert, lookup, size)
import Data.Maybe (isJust, isNothing)
import Utils (splitOnAll)
import Prelude hiding (lookup)

type State = [Bool]

newtype StateChange = StateChange
  { _changes :: [Bool]
  }
  deriving (Show)

type Joltages = [Int]

data Machine = Machine
  { _neededState :: State,
    _buttons :: [StateChange],
    _needed_joltages :: Joltages
  }
  deriving (Show)

interior :: [a] -> [a]
interior = tail . init

parseMachine :: [String] -> Machine
parseMachine tokens = Machine targetState buttons joltage
  where
    tokensWithoutBrackets = map interior tokens
    targetState = map (== '#') . head $ tokensWithoutBrackets
    stateChangeFromWiring buttonWires = StateChange $ map (`elem` buttonWires) [0 .. (length targetState - 1)]
    buttons = map (stateChangeFromWiring . (map read . splitOnAll ',')) $ interior tokensWithoutBrackets
    joltage = map read . splitOnAll ',' $ last tokensWithoutBrackets

parse :: String -> [Machine]
parse = map (parseMachine . splitOnAll ' ') . lines

press :: StateChange -> State -> State
press (StateChange changes) = zipWith xor changes

pressAll :: [StateChange] -> [State] -> [State]
pressAll = liftA2 press

reachAbleStateProgression :: Machine -> [[State]]
reachAbleStateProgression (Machine targetState buttons _) = iterate pressAllButtons [initialState]
  where
    initialState = map (&& False) targetState
    pressAllButtons = nub . pressAll buttons

pressesNeeded :: Machine -> Int
pressesNeeded m@(Machine target _ _) = length . takeWhile (target `notElem`) $ reachAbleStateProgression m

problem1 :: String -> String
problem1 = show . sum . map pressesNeeded . parse

type Cost = Maybe Int -- hack: represent infinity by Nothing

data SearchNode = SearchNode
  { state :: Joltages,
    pressesPerformed :: Int
  }
  deriving (Show)

minPresses :: Joltages -> Joltages -> Cost
minPresses js targets = if any (< 0) distances then Nothing else Just $ maximum distances
  where
    distances = zipWith (-) targets js

press' :: StateChange -> SearchNode -> SearchNode
press' (StateChange changes) (SearchNode joltages presses) = SearchNode newState (presses + 1)
  where
    newState = zipWith (+) (map asInt changes) joltages
      where
        asInt b = if b then 1 else 0

data MinSortedQueue = MSQ
  { _nodes :: [SearchNode],
    _heuristic :: SearchNode -> Cost,
    _visited :: Map Joltages Int
  }

addNode :: SearchNode -> MinSortedQueue -> MinSortedQueue
addNode node q@(MSQ nodes heur visited)
  | isNothing $ heur node = q -- don't insert if we have no hope
  | isJust previousPresses && previousPresses <= Just presses = q -- don't insert if we found a faster way before
  | otherwise =
      MSQ
        (newNodes nodes)
        heur
        (insert joltages presses visited)
  where
    joltages = state node
    presses = pressesPerformed node
    previousPresses = lookup joltages visited
    isSameStateAs = (==) `on` state
    newNodes [] = [node]
    newNodes (n : ns)
      | heur n < heur node = n : newNodes ns
      | otherwise = node : filter (not . isSameStateAs node) (n : ns)

nextNode :: MinSortedQueue -> SearchNode
nextNode (MSQ (s : _) _ _) = s
nextNode _ = error "No search node in queue."

aStarStep :: [StateChange] -> MinSortedQueue -> MinSortedQueue
aStarStep buttons (MSQ (q : qs) h v) = foldl' (flip addNode) (MSQ qs h v) newNodes
  where
    newNodes = map (`press'` q) buttons
aStarStep _ _ = error "Empty search queue. Impossible to perform A* step."

joltageProgression :: Machine -> [MinSortedQueue]
joltageProgression (Machine _ buttons targetJoltage) = iterate' (aStarStep buttons) initialQueue
  where
    joltageHeuristic (SearchNode js p) = liftA2 (+) (Just p) $ minPresses js targetJoltage
    initialNode = SearchNode (map (* 0) targetJoltage) 0
    initialQueue = addNode initialNode (MSQ [] joltageHeuristic empty)

pressesNeeded' :: Machine -> Int
pressesNeeded' m = pressesPerformed finalNode
  where
    (Machine _ _ target) = m
    searchNotFinished = (target /=) . state . nextNode
    finalNode = nextNode . head . dropWhile searchNotFinished . joltageProgression $ m

problem2 :: String -> String
problem2 = const "too slow"
--problem2 = show . sum . map pressesNeeded' . parse


-- DEBUG:
--problem2 s = show . map overview . takeWhile searchNotFinished . joltageProgression $ m1
--  where
--    m1 = head . parse $ s
--    (Machine _ _ target) = m1
--    searchNotFinished = (target /=) . state . nextNode
--    overview q = (size $ _visited q, state $ nextNode q, _heuristic q . nextNode $ q)

