{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Day7 where

-- import Debug.Trace
import Control.Monad (replicateM)
import Data.Attoparsec.Text (Parser, endOfLine, parseOnly, scientific, sepBy)
import Data.Either (fromRight)
import Data.Hashable
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Monoid
import Data.Scientific (coefficient)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

type CalibrationEq = (Int, [Int])

parseInput :: Parser [CalibrationEq]
parseInput = parseLine `sepBy` endOfLine

parseLine :: Parser CalibrationEq
parseLine = do
  testValue <- parseInt <* ": "
  nums <- parseInt `sepBy` " "
  pure (testValue, nums)

parseInt :: Parser Int
parseInt = fromInteger . coefficient <$> scientific

data ExpSymbol = ESInt Int | ESPlus | ESTimes
  deriving (Generic, Eq)
instance Hashable ExpSymbol
instance Show ExpSymbol where
  show (ESInt i) = show i
  show ESPlus = "+"
  show ESTimes = "×"

allSymbols :: Int -> [[ExpSymbol]]
allSymbols n = replicateM n [ESPlus, ESTimes]

(+++) :: [a] -> [a] -> [a]
(+++) a b = (concat . transpose) [a, b]

buildExps :: [Int] -> [[ExpSymbol]]
buildExps xs = fmap (xs' +++) (allSymbols n)
  where
    n = length xs - 1
    xs' = fmap ESInt xs

evalExp :: [ExpSymbol] -> Int
evalExp (ESInt x : xs) = foldr f x (chunksOf 2 $ reverse xs)
  where
    f :: [ExpSymbol] -> Int -> Int
    f [ESInt i, ESPlus] acc = acc + i
    f [ESInt i, ESTimes] acc = acc * i
    f _ _ = minBound
evalExp _ = minBound

solveA :: Text -> Int
solveA txt = getSum $ foldMap f calEq
  where
    f :: (Int, [Int]) -> Sum Int
    f (k, v) =
      if k `elem` map evalExp (buildExps v)
        then Sum k
        else Sum 0 -- neutral for a sum
    calEq = fromRight [] $ parseOnly parseInput txt

data ExpSymbolB = ESIntB Int | ESPlusB | ESTimesB | ESConcatB
  deriving (Generic, Eq)

instance Hashable ExpSymbolB
instance Show ExpSymbolB where
  show (ESIntB i) = show i
  show ESPlusB = "+"
  show ESTimesB = "×"
  show ESConcatB = "||"

allSymbolsB :: Int -> [[ExpSymbolB]]
allSymbolsB n = replicateM n [ESPlusB, ESTimesB, ESConcatB]

buildExpsB :: [Int] -> [[ExpSymbolB]]
buildExpsB xs = fmap (xs' +++) (allSymbolsB n)
  where
    n = length xs - 1
    xs' = fmap ESIntB xs

evalExpB :: [ExpSymbolB] -> Int
evalExpB (ESIntB x : xs) = foldr f x (chunksOf 2 $ reverse xs)
  where
    f :: [ExpSymbolB] -> Int -> Int
    f [ESIntB i, ESPlusB] acc = acc + i
    f [ESIntB i, ESTimesB] acc = acc * i
    f [ESIntB i, ESConcatB] acc = acc * 10 ^ length (show i) + i
    f _ _ = minBound
evalExpB _ = minBound

solveB :: Text -> Int
solveB txt = getSum $ foldMap f calEq
  where
    f :: (Int, [Int]) -> Sum Int
    f (k, v) =
      if k `elem` map evalExpB (buildExpsB v)
        then Sum k
        else Sum 0 -- neutral for a sum
    calEq = fromRight [] $ parseOnly parseInput txt

inputEx :: Text
inputEx =
  T.unlines
    [ "190: 10 19"
    , "3267: 81 40 27"
    , "83: 17 5"
    , "156: 15 6"
    , "7290: 6 8 6 15"
    , "161011: 16 10 13"
    , "192: 17 8 14"
    , "21037: 9 7 18 13"
    , "292: 11 6 16 20"
    ]

main :: IO ()
main = do
  input <- T.readFile "inputs/day7.input"
  putStrLn "Part 1"
  print $ solveA inputEx
  print $ solveA input
  putStrLn "Part 2"
  print $ solveB input
