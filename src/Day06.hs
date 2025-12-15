module Day06 (problem1, problem2) where

import Data.Either
import Utils

splitOnSpaces :: String -> [String]
splitOnSpaces = splitOnAll ' '

type Op = (Integer -> Integer -> Integer)

readOp :: String -> Op
readOp "+" = (+)
readOp "*" = (*)
readOp s = error $ "Parsing error: '" ++ s ++ "' is not a valid op."

data Homework = Homework
  { _numberLines :: [[Integer]],
    _ops :: [Op]
  }

parse :: [Char] -> Homework
parse s = Homework (map (map read) (init inputStrings)) (map readOp $ last inputStrings)
  where
    inputStrings = map splitOnSpaces $ lines s

reduce :: Homework -> Integer
reduce (Homework [] _) = 0
reduce (Homework [l] _) = sum l
reduce (Homework (l1 : l2 : ls) ops) = reduce $ Homework ([o x1 x2 | (o, x1, x2) <- zip3 ops l1 l2] : ls) ops

problem1 :: String -> String
problem1 = show . reduce . parse

data ColumnProblem = ColumnProblem
  { _numbers :: [Integer],
    _op :: Op
  }

extend :: ColumnProblem -> Integer -> ColumnProblem
extend (ColumnProblem nums op) x = ColumnProblem (x : nums) op

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
    baseProblem = let (n, op) = head . rights $ map columnParse cols in ColumnProblem [n] op
    nums = lefts (map columnParse cols)

solve :: ColumnProblem -> Integer
solve (ColumnProblem nums op) = foldl1 op nums

parse2 :: String -> [ColumnProblem]
parse2 = map parseProblem . splitOnBlankColumns . readColumnsLTR . lines

problem2 :: String -> String
problem2 = show . sum . map solve . parse2
