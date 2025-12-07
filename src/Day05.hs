module Day05 (problem1, problem2) where

import Data.Bifunctor (Bifunctor (bimap, first))

data Range = Range
  { left :: Int,
    right :: Int
  }
  deriving (Show)

elem' :: Int -> Range -> Bool
elem' i = liftA2 (&&) ((<= i) . left) ((>= i) . right)

data IngredientData = IngredientData
  { freshRanges :: [Range],
    ingredients :: [Int]
  }
  deriving (Show)

splitOn :: (Eq a) => a -> [a] -> ([a], [a])
splitOn _ [] = ([], [])
splitOn y (x : xs)
  | x == y = ([], xs)
  | otherwise = first ([x] ++) $ splitOn y xs

parse :: String -> IngredientData
parse = uncurry IngredientData . bimap (map parseRange) (map read) . splitOn "" . lines
  where
    parseRange = uncurry Range . bimap read read . splitOn '-'

freshIngredients :: IngredientData -> [Int]
freshIngredients (IngredientData rs is) = filter (\i -> any (elem' i) rs) is

problem1 :: String -> String
problem1 = show . length . freshIngredients . parse

addRange :: Range -> [Range] -> [Range]
addRange r [] = [r]
addRange r (r' : rs)
  | right r < left r' = r : r' : rs -- r fully before r'
  | left r > right r' = r' : addRange r rs -- interval fully after r'
  | otherwise = addRange (Range minLeft maxRight) rs -- r overlaps with r'
  where
    minLeft = min (left r) (left r')
    maxRight = max (right r) (right r')

rangeLen :: Range -> Int
rangeLen (Range l r) = r - l + 1

problem2 :: String -> String
problem2 = show . sum . map rangeLen . foldl (flip addRange) [] . freshRanges . parse
