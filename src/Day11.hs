{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import Control.Monad.State
import Data.Attoparsec.Text (Parser, decimal, parseOnly, sepBy)
import Data.Bifunctor (bimap)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import qualified Data.Text.IO as T

parseInput :: Parser [Int]
parseInput = decimal `sepBy` " "

-- 1. If the stone is engraved with the number 0, it is replaced by a stone
-- engraved with the number 1.
-- 2. If the stone is engraved with a number that has an even number of digits,
-- it is replaced by two stones. The left half of the digits are engraved on
-- the new left stone, and the right half of the digits are engraved on the
-- new right stone. (The new numbers don't keep extra leading zeroes: 1000
-- would become stones 10 and 0.)
-- 3. If none of the other rules apply, the stone is replaced by a new stone; the
-- old stone's number multiplied by 2024 is engraved on the new stone.
applyRule :: Int -> Either Int (Int, Int)
applyRule 0 = Left 1
applyRule n | even leng = Right $ bimap read read $ splitAt half (show n)
  where
    half :: Int
    half = leng `div` 2
    leng :: Int
    leng = length $ show n
applyRule n = Left $ n * 2024

solveA :: [Int] -> Int
solveA = sum . map (\x -> evalState (len 25 x) Map.empty)

-- borrowed from https://functional.computer/blog/memotries
memoize ::
  (Eq a, Ord a, Hashable a, Eq c, Ord c, Hashable c) =>
  (a -> c -> State (HashMap (a, c) b) b) ->
  a ->
  c ->
  State (HashMap (a, c) b) b
memoize f counter val = do
  computed <- get
  case Map.lookup (counter, val) computed of
    Just result ->
      return result
    Nothing -> do
      result <- f counter val
      modify $ Map.insert (counter, val) result
      return result

len :: Int -> Int -> State (HashMap (Int, Int) Int) Int
len 0 _ = pure 1
len i n = case applyRule n of
  Left v -> lenMemo (i - 1) v
  --  trace (unwords [show i, "(", show n, "):", show v, "(", show u, ")"]) u
  Right (v1, v2) -> do
    u1 <- lenMemo (i - 1) v1
    u2 <- lenMemo (i - 1) v2
    pure (u1 + u2)

lenMemo :: Int -> Int -> State (HashMap (Int, Int) Int) Int
lenMemo = memoize len

solveB :: [Int] -> Int
solveB = sum . map (\x -> evalState (len 75 x) Map.empty)

inputEx :: [Int]
inputEx = [125, 17]

main :: IO ()
main = do
  Right input <- parseOnly parseInput <$> T.readFile "inputs/day11.input"
  putStrLn "Part 1"
  print $ solveA inputEx
  print $ solveA input
  putStrLn "Part 2"
  print $ solveB input
