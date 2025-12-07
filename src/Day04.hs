module Day04 (problem1, problem2) where

type PaperGrid = [[Bool]]

type Point = (Int, Int)

isPaper :: Point -> PaperGrid -> Bool
isPaper (x, y) g
  | x < 0 || y < 0 = False
  | y >= length g = False
  | x >= (length . head) g = False
  | otherwise = (g !! y) !! x

parseGrid :: String -> PaperGrid
parseGrid = map (map (== '@')) . lines

paperNeighbours :: Point -> PaperGrid -> [Point]
paperNeighbours (x, y) g = filter (`isPaper` g) [(xn, yn) | xn <- [x - 1 .. x + 1], yn <- [y - 1 .. y + 1], xn /= x || yn /= y]

isAccessible :: Point -> PaperGrid -> Bool
isAccessible p g = (< 4) . length $ paperNeighbours p g

gridCoords :: PaperGrid -> [Point]
gridCoords g = [(x, y) | y <- [0 .. (length g - 1)], x <- [0 .. (length (head g) - 1)]]

accessiblePaper :: PaperGrid -> [Point]
accessiblePaper g = filter (`isAccessible` g) . filter (`isPaper` g) $ gridCoords g

problem1 :: String -> String
problem1 = show . length . accessiblePaper . parseGrid

withoutAccessiblePaper :: PaperGrid -> PaperGrid
withoutAccessiblePaper g = [[isPaper (x, y) g && notElem (x, y) (accessiblePaper g) | x <- [0 .. (length (head g) - 1)]] | y <- [0 .. (length g - 1)]]

removePaper :: (PaperGrid, Int) -> (PaperGrid, Int)
removePaper (g, n) = (withoutAccessiblePaper g, n + length (accessiblePaper g))

problem2 :: String -> String
problem2 s = show . snd . until (null . accessiblePaper . fst) removePaper $ (parseGrid s, 0)
