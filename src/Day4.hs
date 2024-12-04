{-# LANGUAGE OverloadedStrings #-}

module Day4 where

-- import Debug.Trace

import Data.Attoparsec.Text (Parser)
import Data.Either (fromRight)
import Data.List.HT (shearTranspose)
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Data.Text.IO as T

parseInput :: Parser [T.Text]
parseInput = catMaybes <$> P.many1' (P.choice [Just <$> "XMAS",Nothing <$ P.anyChar])

-- .............a...............a b c...rot...a.............................. --
-- .a b...-->...c b.............d e f...--->..d b............................ --
-- .c d.........d...............g h i.........g e c.......................... --
-- ...........................................h f............................ --
-- ...........................................i.............................. --

rot :: Text -> Text
rot = T.unlines . fmap T.pack . shearTranspose . fmap T.unpack . T.lines

-- This is hover near magical incantations
-- I'm like 60% happy with it, it's a bit clumsy.
solveA :: T.Text -> Int
solveA t =
  sum $ fmap (\f -> parseCount $ f t)
    [norm,revnorm,trans,revtrans,rot,rotrev,rottrans,rotrevtrans]
  where
    norm :: Text -> Text
    norm = id
    revnorm = T.reverse
    trans = T.unlines . T.transpose . T.lines
    revtrans = T.reverse . trans
    rotrev = rot . revnorm
    rottrans = rot . T.unlines . reverse . T.lines . trans
    rotrevtrans = rot . trans . T.unlines . reverse . T.lines . trans
    parseCount :: Text -> Int
    parseCount = length . fromRight [] . P.parseOnly parseInput

-- I might end up being wasteful here, leveraging pattern matching
-- .......................................................................... --
-- |.M   S.|.M   M.|.S   M.|.S   S.|......................................... --
-- |.  A  .|.  A  .|.  A  .|.  A  .|......................................... --
-- |.M   S.|.S   S.|.S...M.|.M   M.|......................................... --
-- .......................................................................... --
isMAS :: [[Char]] -> Bool
isMAS [['M',_,'S'],[_,'A',_],['M',_,'S']] = True
isMAS [['M',_,'M'],[_,'A',_],['S',_,'S']] = True
isMAS [['S',_,'M'],[_,'A',_],['S',_,'M']] = True
isMAS [['S',_,'S'],[_,'A',_],['M',_,'M']] = True
isMAS _ = False

all3by3 :: [[Char]] -> [[[Char]]]
all3by3 = catMaybes . (\xs -> fmap (go xs) xs) . index
  where
    index :: [[Char]] -> [(Char,(Int,Int))]
    index = concat . zipWith f [1..] . fmap (`zip` [1..])
    f i = fmap (\(c,n) -> (c,(n,i)))
    go :: [(Char,(Int,Int))] -> (Char,(Int,Int)) -> Maybe [[Char]]
    go xs (_c,(x,y)) = do
      a <- find ((== (x,y)) . snd) xs
      b <- find ((== (x+2,y)) . snd) xs
      c <- find ((== (x+1,y+1)) . snd) xs
      d <- find ((== (x,y+2)) . snd) xs
      e <- find ((== (x+2,y+2)) . snd) xs
      pure [[fst a,'.',fst b],['.',fst c,'.'],[fst d,'.',fst e]]

solveB :: [[Char]] -> Int
solveB = length . filter id . fmap isMAS . all3by3

inputEx :: IsString s => [s]
inputEx = [ "MMMSXXMASM"
          , "MSAMXMSMSA"
          , "AMXSXMAAMM"
          , "MSAMASMSMX"
          , "XMASAMXAMM"
          , "XXAMMXXAMA"
          , "SMSMSASXSS"
          , "SAXAMASAAA"
          , "MAMMMXMMMM"
          , "MXMXAXMASX"
          ]
inputEx2 :: IsString s => [s]
inputEx2 =  [ "......"
            , "..MSMS"
            , "S.MAA."
            , ".ASMSM"
            , "S.M..."
            , "......"
            ]

main :: IO ()
main = do
  input <- T.readFile "inputs/day4.input"
  putStrLn "Part 1"
  print $ solveA $ T.unlines inputEx
  print $ solveA input
  putStrLn "Part 2"
  print $ solveB $ lines . T.unpack $ input
