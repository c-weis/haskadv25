{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import Data.List
import Data.Ord
import Day01
import qualified Day02
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

-- TODO: figure out what's wrong here...
-- assertionFromData :: TestData -> Assertion
-- assertionFromData (TestData solv inp expo) = solver inp @?= expo

-- TODO: figure out how to do autodiscovery & split tests into modules
-- TODO: figure out how to run tests involving IO
testTreeData :: [(String, [TestData])]
testTreeData =
  [ ( "Day01",
      [ TestData
          Day01.problem1
          $(embedStringFile "test/inputs/day01/test01.in")
          "3",
        TestData
          Day01.problem2
          $(embedStringFile "test/inputs/day01/test01.in")
          "6"
      ]
    ),
    ( "Day02",
      [ TestData
          Day02.problem1
          $(embedStringFile "test/inputs/day02/test01.in")
          "1227775554",
        TestData
          Day02.problem2
          $(embedStringFile "test/inputs/day02/test01.in")
          "4174379265"
      ]
    )
  ]

testFromData :: String -> TestData -> TestTree
testFromData name (TestData solv inp expo) = testCase name (solv inp @?= expo)

testGroupFromData :: (String, [TestData]) -> TestTree
testGroupFromData (name, tds) = testGroup name [testFromData (printf "Test%d" idx) test | (idx, test) <- zip [1 :: Int ..] tds]

tests :: TestTree
tests = testGroup "Tests" $ map testGroupFromData testTreeData
