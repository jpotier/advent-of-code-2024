{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day3 where

-- import Debug.Trace

import Data.String
import Data.Scientific (coefficient)
import Data.Attoparsec.Text (Parser)
import Data.Either (fromRight)
import Data.Maybe (catMaybes)
import qualified Data.Attoparsec.Text as P
import qualified Data.Text.IO as T

type Program = [Inst]

data Inst = IMult Integer Integer | IDo | IDont
  deriving Show

parseInput :: Parser Program
parseInput =
  catMaybes <$>
    P.many1' (P.choice [ Just <$> parseMult
                       , Just <$> parseDo
                       , Just <$> parseDont
                       , Nothing <$ P.anyChar])
  <* P.endOfInput

parseDo :: Parser Inst
parseDo = IDo <$ "do()"

parseDont :: Parser Inst
parseDont = IDont <$ "don't()"

parseMult :: Parser Inst
parseMult = do
  _ <- "mul("
  n1 <- toInteger . coefficient <$> P.scientific
  _ <- ","
  n2 <- toInteger . coefficient <$> P.scientific
  _ <- ")"
  pure $ IMult n1 n2

solveA :: Program -> Integer
solveA = sum . fmap mult

mult :: Inst -> Integer
mult (IMult n1 n2) = n1 * n2
mult _ = 0

solveB :: Program -> Integer
solveB = sum . fmap mult . cleanup
  where
    cleanup (x@(IMult _ _):xs) = x:cleanup xs
    cleanup (IDo:xs) = cleanup xs
    cleanup (IDont:xs) = forget xs
    cleanup [] = []
    forget ((IMult _ _):xs) = forget xs
    forget (IDo:xs) = cleanup xs
    forget (IDont:xs) = forget xs
    forget [] = []

input :: IsString s => s
input = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

main :: IO ()
main = do
  input <- T.readFile "inputs/day3.input"
  putStrLn "Part 1"
  let xs = fromRight [] $ P.parseOnly parseInput input
  print $ solveA xs
  putStrLn "Part 2"
  print $ solveB xs
