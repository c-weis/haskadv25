{-# LANGUAGE TupleSections #-}

module Day09 (problem1, problem2) where

import Data.Function (on)
import Data.List (sortBy)
import Utils

type Pos = (Int, Int)

data Rect = Rect
  { _p1 :: Pos,
    _p2 :: Pos
  }
  deriving (Show)

area :: Rect -> Int
area (Rect (x1, y1) (x2, y2)) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

parse :: String -> [Pos]
parse =
  let parseLine = onBoth read . splitOnFirst ','
   in map parseLine . lines

rects :: [Pos] -> [Rect]
rects = map rectFromTuple . unorderedDistinctPairs
  where
    rectFromTuple = uncurry Rect

problem1 :: String -> String
problem1 = show . maximum . map area . rects . parse

data Bound
  = LowerBound {_l :: Int}
  | UpperBound {_h :: Int}
  | Free

meets :: Int -> Bound -> Bool
meets _ Free = True
meets x (LowerBound l) = x >= l
meets x (UpperBound h) = x <= h

meetsStrictly :: Int -> Bound -> Bool
meetsStrictly _ Free = True
meetsStrictly x (LowerBound l) = x > l
meetsStrictly x (UpperBound h) = x < h

type Range = [Bound]

elemR :: Int -> Range -> Bool
elemR x = all (meets x)

elemInteriorR :: Int -> Range -> Bool
elemInteriorR x = all (meetsStrictly x)

data Segment
  = VerticalLine
      { _x :: Int,
        _yRange :: Range
      }
  | HorizontalLine
      { _xRange :: Range,
        _y :: Int
      }

elemSeg :: Pos -> Segment -> Bool
elemSeg (x, y) (VerticalLine xl yR) = x == xl && y `elemR` yR
elemSeg (x, y) (HorizontalLine xR yl) = x `elemR` xR && y == yl

newtype Carpet = Carpet
  { _edges :: [Segment]
  }

straightLine :: Pos -> Pos -> Segment
straightLine p1@(x1, y1) p2@(x2, y2)
  | x1 == x2 = VerticalLine x1 (makeRange y1 y2)
  | y1 == y2 = HorizontalLine (makeRange x1 x2) y1
  | otherwise = error $ "Points don't share either coordinate: " ++ show (p1, p2)
  where
    makeRange a b = [LowerBound $ min a b, UpperBound $ max a b]

intersect :: Segment -> Segment -> Maybe Pos
intersect (VerticalLine _ _) (VerticalLine _ _) = Nothing
intersect (HorizontalLine _ _) (HorizontalLine _ _) = Nothing
intersect v@(VerticalLine _ _) h@(HorizontalLine _ _) = intersect h v
intersect (HorizontalLine xR y) (VerticalLine x yR) =
  if x `elemR` xR && y `elemR` yR
    then Just (x, y)
    else Nothing

elemCarpetEdge :: Pos -> Carpet -> Bool
elemCarpetEdge p (Carpet edges) = any (p `elemSeg`) edges

elemCarpet :: Pos -> Carpet -> Bool
elemCarpet p@(x, y) c@(Carpet edges) = (onEdge ||) $ even . length . filter (/= Nothing) . map (intersect testRay) $ edges
  where
    onEdge = elemCarpetEdge p c
    testRay = HorizontalLine [LowerBound x] y

pointsBetween :: (Pos, Pos) -> [Pos]
pointsBetween (p1@(x1, y1), p2@(x2, y2))
  | x1 == x2 = map (x1,) [y1 .. y2]
  | y1 == y2 = map (,y1) [x1 .. x2]
  | otherwise = error $ "Points don't share either coordinate: " ++ show (p1, p2)

connectInsideCarpet :: (Pos, Pos) -> Carpet -> Bool
connectInsideCarpet pointPair c = all (`elemCarpet` c) $ pointsBetween pointPair

roll :: [a] -> [a]
roll xs = last xs : init xs

allEdgesInsideCarpet :: Rect -> Carpet -> Bool
allEdgesInsideCarpet (Rect (x1, y1) (x2, y2)) c = all (`connectInsideCarpet` c) rectEdgePairs
  where
    orderedPts = [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]
    rectEdgePairs = zip orderedPts (roll orderedPts)

-- TBD
problem2 :: String -> String
problem2 = const "nope"

-- problem2 s = show . area . head . filter (`allEdgesInsideCarpet` carpet) . sortBy (compare `on` negate . area) $ candidateRects
--  where
--    corners = parse s
--    carpet = Carpet $ zipWith straightLine corners $ roll corners
--    candidateRects = rects corners
