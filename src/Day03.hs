module Day03 (problem1, problem2) where

type Bank = [Int]

digitsToNum :: (Num a) => (a, a) -> a
digitsToNum (x, y) = 10 * x + y

rightOf :: (Eq a) => a -> [a] -> [a]
rightOf _ [] = []
rightOf y (x : xs)
  | x == y = xs
  | otherwise = rightOf y xs

largestLeft :: Bank -> Int
largestLeft = maximum . init

maxJoltage :: Bank -> Int
maxJoltage b = digitsToNum (largestLeft b, maximum (rightOf (largestLeft b) b))

readDigit :: Char -> Int
readDigit = read . (: [])

banks :: String -> [Bank]
banks = map (map readDigit) . lines

problem1 :: String -> String
problem1 = show . sum . map maxJoltage . banks

dropFinal :: Int -> [a] -> [a]
dropFinal 0 = id
dropFinal n = dropFinal (n - 1) . init

data SearchState = SearchState
  { joltage :: Int,
    digitsMissing :: Int,
    bankRemaining :: Bank
  }

searchJoltage :: SearchState -> Int
searchJoltage (SearchState j 0 _) = j
searchJoltage (SearchState j d b) =
  searchJoltage
    SearchState
      { joltage = 10 * j + maxNext,
        digitsMissing = d - 1,
        bankRemaining = rightOf maxNext b
      }
  where
    maxNext = maximum $ dropFinal (d - 1) b

maxLongJoltage :: Int -> Bank -> Int
maxLongJoltage digs bank =
  searchJoltage
    SearchState
      { joltage = 0,
        digitsMissing = digs,
        bankRemaining = bank
      }

problem2 :: String -> String
problem2 = show . sum . map (maxLongJoltage 12) . banks
