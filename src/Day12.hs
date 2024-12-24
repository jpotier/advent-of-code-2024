{-# LANGUAGE OverloadedStrings #-}

module Day12 where

-- import Debug.Trace

import Data.Attoparsec.Text (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

parseInput :: Parser a
parseInput = undefined

solveA :: (Show a) => a -> a
solveA = undefined

solveB :: (Show a) => a -> a
solveB = undefined

inputEx :: Text
inputEx =
  T.unlines
    [ "AAAA"
    , "BBCD"
    , "BBCC"
    , "EEEC"
    ]

inputEx2 :: Text
inputEx2 =
  T.unlines
    [ "OOOOO"
    , "OXOXO"
    , "OOOOO"
    , "OXOXO"
    , "OOOOO"
    ]

inputEx3 :: Text
inputEx3 =
  T.unlines
    [ "RRRRIICCFF"
    , "RRRRIICCCF"
    , "VVRRRCCFFF"
    , "VVRCCCJFFF"
    , "VVVVCJJCFE"
    , "VVIVCCJJEE"
    , "VVIIICJJEE"
    , "MIIIIIJJEE"
    , "MIIISIJEEE"
    , "MMMISSJEEE"
    ]

main :: IO ()
main = do
  input <- T.readFile "inputs/day12.input"
  putStrLn "Part 1"
  print $ solveA inputEx
  print $ solveA input
  putStrLn "Part 2"
  print $ solveB input
