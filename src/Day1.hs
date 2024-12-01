{-# LANGUAGE OverloadedStrings #-}

module Day1 where

-- import Debug.Trace

import qualified Data.Attoparsec.Text as P
import qualified Data.Text.IO as T
import Data.Scientific (coefficient)
import Data.List (sort)
import Data.Bifunctor (bimap)
import Data.Either (fromRight)

parseInput :: P.Parser [(Integer,Integer)]
parseInput = parseLine `P.sepBy` P.endOfLine

parseLine :: P.Parser (Integer,Integer)
parseLine = do
  num1 <- coefficient <$> P.scientific
  _ <- P.many1 P.space
  num2 <- coefficient <$> P.scientific
  pure (num1,num2)

-- calculating distance
solveA :: [(Integer,Integer)] -> Integer
solveA = sum
       . fmap (\(x,y) -> abs (y-x))
       . uncurry zip
       . bimap sort sort
       . unzip

-- calculating similarity score
solveB :: [(Integer,Integer)] -> Integer
solveB xs = sum ys
  where
    ys = uncurry similarity $ unzip xs
    similarity as bs = map go as
      where
        -- Find how many occurences n of a in bs
        -- Return a*n
        go :: Integer -> Integer
        go a = a * toInteger (length (filter (== a) bs))

main :: IO ()
main = do
  _input <- T.readFile "inputs/day1-example.input"
  input <- T.readFile "inputs/day1.input"
  putStrLn "Part 1"
  let xs = fromRight [] $ P.parseOnly parseInput input
  print $ solveA xs
  putStrLn "Part 2"
  print $ solveB xs
