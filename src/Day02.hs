module Day02 (problem1, problem2) where

import Data.List.Split

dropHalf :: [a] -> [a]
dropHalf xs = drop (div (length xs) 2) xs

evenLength :: String -> Bool
evenLength = even . length

isDoubled :: String -> Bool
isDoubled s = evenLength s && and [a == b | (a, b) <- zip s (dropHalf s)]

tupler :: [a] -> (a, a)
tupler xs = (head xs, head $ tail xs)

rangeFromTuple :: (Int, Int) -> [Int]
rangeFromTuple (x, y) = [x .. y]

ranger :: String -> [Int]
ranger range_text = rangeFromTuple . tupler $ map read (splitOn "-" range_text)

numbers :: String -> [Int]
numbers = concatMap ranger . splitOn ","

problem1 :: String -> String
problem1 s = show $ sum $ filter (isDoubled . show) (numbers s)

strides :: Int -> [Int]
strides n = filter ((0 ==) . rem n) [1 .. (n - 1)]

containsRepeats :: String -> Bool
containsRepeats s = or [and [x == y | (x, y) <- zip s (drop stride s)] | stride <- strides $ length s]

problem2 :: String -> String
problem2 s = show $ sum $ filter (containsRepeats . show) (numbers s)
