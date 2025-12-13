module Day06 (problem1, problem2) where

import Data.Bifunctor
import Data.Either

type Pred a = (a -> Bool)

empty :: [a] -> Bool
empty [] = True
empty _ = False

is :: (Eq a) => a -> Pred a
is = (==)

splitWhen :: Pred a -> [a] -> ([a], [a])
splitWhen _ [] = ([], [])
splitWhen p (y : ys)
  | p y = ([], ys)
  | otherwise = first ([y] ++) (splitWhen p ys)

splitWhenever :: Pred a -> [a] -> [[a]]
splitWhenever p ys = filter (not . empty) $ map fst $ takeWhile ((not . empty) . uncurry (++)) $ iterate (splitWhen p . snd) ([], ys)

splitOnFirst :: (Eq a) => a -> [a] -> ([a], [a])
splitOnFirst = splitWhen . is

splitOnAll :: (Eq a) => a -> [a] -> [[a]]
splitOnAll = splitWhenever . is

splitOnSpaces :: String -> [String]
splitOnSpaces = splitOnAll ' '

type Op = (Integer -> Integer -> Integer)

readOp :: String -> Op
readOp "+" = (+)
readOp "*" = (*)
readOp s = error $ "Parsing error: '" ++ s ++ "' is not a valid op."

data Homework = Homework
  { numberLines :: [[Integer]],
    ops :: [Op]
  }

parse :: [Char] -> Homework
parse s = Homework (map (map read) (init inputStrings)) (map readOp $ last inputStrings)
  where
    inputStrings = map splitOnSpaces $ lines s

reduce :: Homework -> Integer
reduce (Homework [] _) = 0
reduce (Homework [l] _) = sum l
reduce (Homework (l1 : l2 : ls) os) = reduce $ Homework ([o x1 x2 | (o, x1, x2) <- zip3 os l1 l2] : ls) os

problem1 :: String -> String
problem1 = show . reduce . parse

data ColumnProblem = ColumnProblem
  { numbers :: [Integer],
    op :: Op
  }

extend :: ColumnProblem -> Integer -> ColumnProblem
extend (ColumnProblem xs o) x = ColumnProblem (x : xs) o

readColumnsLTR :: [[a]] -> [[a]]
readColumnsLTR [] = []
readColumnsLTR xss = [x | (x : _) <- xss] : readColumnsLTR [xs | (_ : xs) <- xss]

blank :: String -> Bool
blank = all (== ' ')

splitOnBlankColumns :: [String] -> [[String]]
splitOnBlankColumns = splitWhenever blank

columnParse :: String -> Either Integer (Integer, Op)
columnParse s
  | last s == '*' = Right (read $ init s, (*))
  | last s == '+' = Right (read $ init s, (+))
  | otherwise = Left $ read s

parseProblem :: [String] -> ColumnProblem
parseProblem cols = foldl extend baseProblem nums
  where
    baseProblem = let (n, o) = head . rights $ map columnParse cols in ColumnProblem [n] o
    nums = lefts (map columnParse cols)

solve :: ColumnProblem -> Integer
solve (ColumnProblem ns o) = foldl1 o ns

parse2 :: String -> [ColumnProblem]
parse2 = map parseProblem . splitOnBlankColumns . readColumnsLTR . lines

problem2 :: String -> String
--problem2 = show . readColumnsLTR . lines
problem2 = show . sum . map solve . parse2
