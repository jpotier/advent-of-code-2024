{-# LANGUAGE OverloadedStrings #-}

module Day2 where

-- import Debug.Trace

import Data.Scientific (coefficient)
import Data.Attoparsec.Text (Parser)
import Data.Either (fromRight)
import Data.List (subsequences)
import qualified Data.Attoparsec.Text as P
import qualified Data.Text.IO as T

type Report = [Integer]

parseInput :: Parser [Report]
parseInput = (coefficient <$> P.scientific) `P.sepBy1` " " `P.sepBy1` P.endOfLine

-- Count how many reports are safe
solveA :: [Report] -> Integer
solveA = toInteger . length . filter id . fmap isSafe

-- The levels are either all increasing or all decreasing.
-- Any two adjacent levels differ by at least one and at most three.
isSafe :: Report -> Bool
isSafe = check . reportDiff
  where
    check dx = (all (>= 0) dx || all (<= 0) dx)
            && all (\d -> 1 <= abs d && abs d <= 3) dx

reportDiff :: Report -> [Integer]
reportDiff r = tail $ zipWith (-) r (0:r)

-- Count how many reports are safe, if one can remove up to one
-- A report can be safe directly OR it can be safe if any of its levels can be
-- removed to make it safe.
solveB :: [Report] -> Integer
solveB = toInteger . length . filter id . fmap isSafeOrAboutSafe

isSafeOrAboutSafe :: Report -> Bool
isSafeOrAboutSafe r = isSafe r || solveA dropOneLevel > 0
  where
    dropOneLevel :: [Report]
    dropOneLevel = filter ((== (l-1)) . length) $ subsequences r
    l = length r

main :: IO ()
main = do
  _input <- T.readFile "inputs/day2-example.input"
  input <- T.readFile "inputs/day2.input"
  putStrLn "Part 1"
  let xs = fromRight [] $ P.parseOnly parseInput input
  print $ solveA xs
  putStrLn "Part 2"
  print $ solveB xs
