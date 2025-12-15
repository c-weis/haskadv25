{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import Data.List
import Data.Ord
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

main = defaultMain tests

type ProblemSolver = String -> String

data TestData = TestData
  { solver :: ProblemSolver,
    input :: String,
    exp_output :: String
  }

-- TODO: figure out how to do autodiscovery & split tests into modules
-- TODO: figure out how to run tests involving IO
testTreeData :: [(String, [TestData])]
testTreeData =
  [ ( "Day01",
      [ TestData
          Day01.problem1
          $(embedStringFile "test/inputs/day01.test.in")
          "3",
        TestData
          Day01.problem2
          $(embedStringFile "test/inputs/day01.test.in")
          "6"
      ]
    ),
    ( "Day02",
      [ TestData
          Day02.problem1
          $(embedStringFile "test/inputs/day02.test.in")
          "1227775554",
        TestData
          Day02.problem2
          $(embedStringFile "test/inputs/day02.test.in")
          "4174379265"
      ]
    ),
    ( "Day03",
      [ TestData
          Day03.problem1
          $(embedStringFile "test/inputs/day03.test.in")
          "357",
        TestData
          Day03.problem2
          $(embedStringFile "test/inputs/day03.test.in")
          "3121910778619"
      ]
    ),
    ( "Day04",
      [ TestData
          Day04.problem1
          $(embedStringFile "test/inputs/day04.test.in")
          "13",
        TestData
          Day04.problem2
          $(embedStringFile "test/inputs/day04.test.in")
          "43"
      ]
    ),
    ( "Day05",
      [ TestData
          Day05.problem1
          $(embedStringFile "test/inputs/day05.test.in")
          "3",
        TestData
          Day05.problem2
          $(embedStringFile "test/inputs/day05.test.in")
          "14"
      ]
    ),
    ( "Day06",
      [ TestData
          Day06.problem1
          $(embedStringFile "test/inputs/day06.test.in")
          "4277556",
        TestData
          Day06.problem2
          $(embedStringFile "test/inputs/day06.test.in")
          "3263827"
      ]
    ),
    ( "Day07",
      [ TestData
          Day07.problem1
          $(embedStringFile "test/inputs/day07.test.in")
          "21",
        TestData
          Day07.problem2
          $(embedStringFile "test/inputs/day07.test.in")
          "40"
      ]
    ),
    ( "Day08",
      [ TestData
          Day08.problem1Test
          $(embedStringFile "test/inputs/day08.test.in")
          "40",
        TestData
          Day08.problem2
          $(embedStringFile "test/inputs/day08.test.in")
          "25272"
      ]
    )
  ]

testFromData :: String -> TestData -> TestTree
testFromData name (TestData solv inp expo) = testCase name (solv inp @?= expo)

testGroupFromData :: (String, [TestData]) -> TestTree
testGroupFromData (name, tds) = testGroup name [testFromData (printf "Test%d" idx) test | (idx, test) <- zip [1 :: Int ..] tds]

tests :: TestTree
tests = testGroup "Tests" $ map testGroupFromData testTreeData
