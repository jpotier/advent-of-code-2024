{-# LANGUAGE OverloadedStrings #-}

module Template where

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
inputEx = T.unlines ["a"]

main :: IO ()
main = do
  input <- T.readFile "inputs/dayXX.input"
  putStrLn "Part 1"
  print $ solveA inputEx
  print $ solveA input
  putStrLn "Part 2"
  print $ solveB input
