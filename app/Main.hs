module Main where

import Day01
import Day02
import Options.Applicative
import Text.Printf

dayparser :: Parser Int
dayparser =
  option
    auto
    (long "day" <> short 'd' <> metavar "DAY" <> help "Run day DAY")

partparser :: Parser Int
partparser =
  option
    auto
    (long "part" <> short 'p' <> metavar "PART" <> help "Run part PART")

data ProblemSpec = ProblemSpec
  {day :: Int, part :: Int}

parser :: Parser ProblemSpec
parser =
  ProblemSpec
    <$> dayparser
    <*> partparser

opts :: ParserInfo ProblemSpec
opts =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc "Runs a solution of day DAY, part PART of Advent of Code 2025."
        <> header "haskell-advent2025"
    )

message :: Int -> Int -> IO ()
message = printf "Running day %d part %d...\n"

-- TODO: figure out if this can be done w metaprogramming
-- TODO: consider separate module for this?

data Setup = Setup
  {solver :: String -> String, filePath :: String}

setupList :: [(Setup, Setup)]
setupList = [
  (Setup Day01.problem1 "inputs/day01/input01.in", Setup Day01.problem2 "inputs/day01/input01.in"),
  (Setup Day02.problem1 "inputs/day02/input01.in", Setup Day02.problem2 "inputs/day02/input01.in")
  ]

setup :: Int -> Int -> Setup
setup d p
  | p == 1 = fst $ setupList !! (d - 1)
  | p == 2 = snd $ setupList !! (d - 1)
  | otherwise = error "Invalid part specified: %d\n" part

main :: IO ()
main = do
  ProblemSpec d p <- execParser opts
  message d p
  content <- readFile $ filePath (setup d p)
  printf $ solver (setup d p) content ++ "\n"
