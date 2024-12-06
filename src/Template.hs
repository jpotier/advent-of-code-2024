{-# LANGUAGE OverloadedStrings #-}

module Template where

-- import Debug.Trace

import Data.Attoparsec.Text (Parser)
-- import Data.Either (fromRight)
-- import Data.List.HT (shearTranspose)
-- import Data.List (find)
-- import Data.Maybe (catMaybes)
import Data.String (IsString)
-- import Data.Text (Text)
-- import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Data.Text.IO as T

parseInput :: Parser a
parseInput = undefined

solveA :: Show a => a -> a
solveA = undefined

solveB :: Show a => a -> a
solveB = undefined

inputEx :: IsString s => [s]
inputEx = ["a"]

main :: IO ()
main = do
  input <- T.readFile "inputs/template.input"
  putStrLn "Part 1"
  print $ solveA $ T.unlines inputEx
  print $ solveA input
  putStrLn "Part 2"
  print $ solveB input
