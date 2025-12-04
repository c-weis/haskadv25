{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import Data.List
import Data.Ord
import Day01
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [day01]

-- TODO: figure out how to do autodiscovery & split tests into modules
-- TODO: figure out how to run tests involving IO
test_data :: (Int, Int) -> [(String, String)]
test_data (1, 1) = [($(embedStringFile "test/inputs/day01/test01.in"), "3")]
test_data (1, 2) = [($(embedStringFile "test/inputs/day01/test01.in"), "6")]

day01 =
  testGroup
    "Day01"
    [day01p1, day01p2]

day01p1 =
  testGroup
    "Day01 Part1"
    [ testCase
        ("Day01 Part1 Test" ++ show idx)
        ( Day01.problem1 inp
            @?= exp_out
        )
      | (idx, (inp, exp_out)) <- zip [1 ..] $ test_data (1, 1)
    ]

day01p2 =
  testGroup
    "Day01 Part2"
    [ testCase
        ("Day01 Part2 Test" ++ show idx)
        ( Day01.problem2 inp
            @?= exp_out
        )
      | (idx, (inp, exp_out)) <- zip [1 ..] $ test_data (1, 2)
    ]