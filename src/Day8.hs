{-# LANGUAGE OverloadedStrings #-}

module Day8 where

-- import Debug.Trace
import Data.Attoparsec.Text (
  Parser,
  endOfLine,
  many1,
  parseOnly,
  satisfy,
  sepBy,
 )
import Data.Char (isAlphaNum, isAscii)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as S
import Data.List (tails)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Float (int2Float)

type Antennas = H.HashMap Char [Pos]
type Antinodes = S.HashSet Pos
type Pos = (Int, Int)
type Bounds2D = (Pos, Pos)

parseInput :: Parser (Antennas, Bounds2D)
parseInput = do
  xs <- concat . flip (zipWith f) [0 ..] . reverse <$> parseLine `sepBy` endOfLine
  pure (toAntennas xs, findBounds xs)
  where
    f :: [(Char, Int)] -> Int -> [(Char, [Pos])]
    f xs i = map (\(a, b) -> (a, [(b, i)])) xs
    toAntennas = H.fromListWith (++) . filter (('.' /=) . fst)
    findBounds :: [(Char, [Pos])] -> Bounds2D
    findBounds = (\xs -> (minimum xs, maximum xs)) . concat . H.fromListWith (++)

parseLine :: Parser [(Char, Int)]
parseLine =
  (`zip` [0 ..])
    <$> many1 (satisfy (\c -> (isAscii c && isAlphaNum c) || (c == '.')))

allPairs :: [a] -> [(a, a)]
allPairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

findAntinodes :: [Pos] -> Antinodes
findAntinodes = foldMap findAntinode . allPairs

findAntinode :: (Pos, Pos) -> Antinodes
findAntinode ((x1, y1), (x2, y2)) =
  S.fromList
    [ (x2 + (x2 - x1), y2 + (y2 - y1))
    , (x1 - (x2 - x1), y1 - (y2 - y1))
    ]

inBounds :: Pos -> Pos -> Pos -> Bool
inBounds (xmin, ymin) (xmax, ymax) (x, y) =
  xmin <= x && x <= xmax && ymin <= y && y <= ymax

solveA :: Text -> Either String Int
solveA txt = do
  (antennas, bounds) <- parseOnly parseInput txt
  let allAN = foldMap findAntinodes antennas
  pure $ S.size $ S.filter (uncurry inBounds bounds) allAN

findAntinodes' :: Bounds2D -> [Pos] -> Antinodes
findAntinodes' b = foldMap (findAntinode' b) . allPairs

max' :: Bounds2D -> Int
max' ((xmin, ymin), (xmax, ymax)) =
  ceiling $
    sqrt
      ( (int2Float xmax - int2Float xmin) ^ (2 :: Integer)
          + (int2Float ymax - int2Float ymin) ^ (2 :: Integer)
      )

findAntinode' :: Bounds2D -> (Pos, Pos) -> Antinodes
findAntinode' b ((x1, y1), (x2, y2)) =
  S.filter (uncurry inBounds b) $
    S.fromList $
      concatMap go [0 .. (max' b)]
  where
    go n =
      [ (x2 + n * (x2 - x1), y2 + n * (y2 - y1))
      , (x1 - n * (x2 - x1), y1 - n * (y2 - y1))
      ]

solveB :: Text -> Either String Int
solveB txt = do
  (antennas, bounds) <- parseOnly parseInput txt
  let allAN = foldMap (findAntinodes' bounds) antennas
  pure $ S.size $ S.filter (uncurry inBounds bounds) allAN

inputEx :: (IsString s) => [s]
inputEx =
  [ "............"
  , "........0..."
  , ".....0......"
  , ".......0...."
  , "....0......."
  , "......A....."
  , "............"
  , "............"
  , "........A..."
  , ".........A.."
  , "............"
  , "............"
  ]

main :: IO ()
main = do
  input <- T.readFile "inputs/day8.input"
  putStrLn "Part 1"
  print $ solveA $ T.unlines inputEx
  print $ solveA input
  putStrLn "Part 2"
  print $ solveB $ T.unlines inputEx
  print $ solveB input
