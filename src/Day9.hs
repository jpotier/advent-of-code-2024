{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import Data.Attoparsec.Text (Parser, digit, many1, parseOnly)
import Data.Bifunctor (second)
import Data.Either (fromRight)
import Data.Int (Int8)
import Data.List (foldl', partition)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Vector as V

type DiskMap = V.Vector Int8
type DiskContents = V.Vector (Maybe Int)

parseInput :: Parser DiskMap
parseInput = V.fromList <$> many1 parseInt8

parseInt8 :: Parser Int8
parseInt8 = read . (: []) <$> digit

expandDiskMap :: DiskMap -> DiskContents
expandDiskMap = V.fromList . concat . V.imap go
  where
    go :: Int -> Int8 -> [Maybe Int]
    go i n | even i = replicate (fromIntegral n) $ Just (i `div` 2)
    go _i n = replicate (fromIntegral n) Nothing

checksum :: DiskContents -> Int
checksum = V.sum . V.imap go
  where
    go :: Int -> Maybe Int -> Int
    go _ Nothing = 0
    go i (Just v) = i * v

defrag :: DiskContents -> DiskContents
defrag dc = V.update dc (fill V.++ erase)
  where
    empty :: V.Vector Int -- all empty positions left to right
    empty = V.elemIndices Nothing dc
    nonEmpty :: V.Vector Int -- all non empty positions right to left
    nonEmpty = V.reverse $ V.findIndices isJust dc
    rewriteLength = max (V.length empty) (V.length nonEmpty)
    fill :: V.Vector (Int, Maybe Int)
    fill = V.map go (V.zip empty nonEmpty)
    go (eI, neI) = (eI, dc V.! neI) -- replace empty spot with non-empty
    erase :: V.Vector (Int, Maybe Int)
    erase = V.fromList [(i, Nothing) | i <- [rewriteLength .. V.length dc - 1]]

showDiskContents :: DiskContents -> String
showDiskContents = V.toList . fmap go
  where
    go Nothing = '.'
    go (Just v) = head (show v)

solveA :: Text -> Int
solveA txt = checksum . defrag . expandDiskMap $ inputDiskMap
  where
    inputDiskMap = fromRight mempty $ parseOnly parseInput txt

-- Now do the same again but instead of moving blocks, move files as a whole
defragB :: DiskContents -> DiskContents
defragB dc = V.update dc updates
  where
    (empty, nEmpty) =
      second reverse
        . partition ((== Nothing) . snd . (V.! 0))
        . V.groupBy (\x y -> snd x == snd y)
        . V.indexed
        $ dc
    updates :: V.Vector (Int, Maybe Int)
    updates = V.fromList . snd $ foldl' go (empty, []) nEmpty
    go ::
      ([V.Vector (Int, Maybe Int)], [(Int, Maybe Int)]) ->
      V.Vector (Int, Maybe Int) ->
      ([V.Vector (Int, Maybe Int)], [(Int, Maybe Int)])
    go (e : ex, out) ne
      | V.length ne <= V.length e = (e' : ex, swapFile ne e ++ out)
      where
        e' :: V.Vector (Int, Maybe Int)
        e' = V.drop (V.length ne) e
    go ([], out) _ = ([], out)
    go (e : ex, out) ne = (e : ex', out')
      where
        (ex', out') = go (ex, out) ne

-- generate update instruction to swap files
-- it takes care of filing only the necessary space
-- only swaps when trying to move a file "to the left"
swapFile ::
  V.Vector (Int, Maybe Int) -> V.Vector (Int, Maybe Int) -> [(Int, Maybe Int)]
swapFile vne ve = concatMap swapIndex $ V.toList $ V.zip vne ve
  where
    swapIndex ((i1, v1), (i2, v2)) | i1 > i2 = [(i1, v2), (i2, v1)]
    swapIndex _ = []

solveB :: Text -> Int
solveB txt = checksum . defragB . expandDiskMap $ inputDiskMap
  where
    inputDiskMap = fromRight mempty $ parseOnly parseInput txt

inputEx :: Text
inputEx = "2333133121414131402"

main :: IO ()
main = do
  input <- T.readFile "inputs/day9.input"
  putStrLn "Part 1"
  print $ solveA inputEx
  print $ solveA input
  putStrLn "Part 2"
  print $ solveB inputEx
  print $ solveB input
