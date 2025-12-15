{-# LANGUAGE TupleSections #-}

module Utils where

import Data.Bifunctor (first)

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
  | otherwise = first (y :) (splitWhen p ys)

splitWhenever :: Pred a -> [a] -> [[a]]
splitWhenever p ys = filter (not . empty) $ map fst $ takeWhile ((not . empty) . uncurry (++)) $ iterate (splitWhen p . snd) ([], ys)

splitOnFirst :: (Eq a) => a -> [a] -> ([a], [a])
splitOnFirst = splitWhen . is

splitOnAll :: (Eq a) => a -> [a] -> [[a]]
splitOnAll = splitWhenever . is

onBoth :: (a -> b) -> (a, a) -> (b, b)
onBoth f (x, y) = (f x, f y)

unorderedDistinctPairs :: [a] -> [(a, a)]
unorderedDistinctPairs [] = []
unorderedDistinctPairs (x : xs) = map (x,) xs ++ unorderedDistinctPairs xs