{-# LANGUAGE OverloadedStrings #-}

module Day5 where

-- import Debug.Trace
import Data.Attoparsec.Text (Parser)
import Data.Either (fromRight)
import Data.List (elemIndex)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid (Sum(Sum))
import Data.Scientific (coefficient)
import Data.Semigroup (getSum)
import Data.Text (Text)
import qualified Algebra.Graph.Acyclic.AdjacencyMap as AG
import qualified Algebra.Graph.AdjacencyMap as G
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Data.Text.IO as T

data SafetyProtocol = SafetyProtocol
  { spPor :: [OrderingRule]
  , spPtp :: [Update]
  }
  deriving Show

type OrderingRule = (Int,Int)
type Update = [Int]

parseInput :: Parser SafetyProtocol
parseInput = do
  por <- parsePOR `P.sepBy` P.endOfLine
  _ <- P.count 2 P.endOfLine
  ptp <- parsePTP `P.sepBy` P.endOfLine
  pure $ SafetyProtocol por ptp

int :: Parser Int
int = fromInteger . coefficient <$> P.scientific

parsePOR :: Parser OrderingRule
parsePOR = do
  n1 <- int <* "|"
  n2 <- int
  pure (n1,n2)

parsePTP :: Parser Update
parsePTP = int `P.sepBy1` ","

-- Try each ordering rule on one update, to see if it works
check :: Update -> [OrderingRule] -> Maybe Update
check u = (\xs -> if and xs then Just u else Nothing) . map (`check'` u)

check' :: OrderingRule -> Update -> Bool
check' (a,b) xs = fromMaybe True $ do
  i <- elemIndex a xs
  j <- elemIndex b xs
  pure $ i < j

specialSum :: [Update] -> Int
specialSum = getSum . foldMap (\xs -> Sum $ xs !! (length xs `div` 2))

solveA :: SafetyProtocol -> Int
solveA (SafetyProtocol por ptp) = specialSum $ mapMaybe (`check` por) ptp

fix :: Update -> [OrderingRule] -> Maybe Update
fix u xs = case reorder of
  ys | ys == u -> Nothing
  ys | otherwise -> Just ys
  where
    -- full set of rules is a cyclical graph. But if you restrict to the numbers
    -- that show in an Update, then it works.
    xs' = tSort $ filter (\(a,b) -> a `elem` u && b `elem` u) xs
    -- pick all the elements of Update from the topologically sorted graph
    -- you get a sorted Update
    reorder = filter (`elem` u) xs'

tSort :: [OrderingRule] -> [Int]
tSort = AG.topSort . fromMaybe AG.empty . AG.toAcyclic . G.edges

check2 :: Update -> [OrderingRule] -> Maybe Update
check2 u = (\xs -> if and xs then Nothing else Just u) . map (`check'` u)

solveB :: SafetyProtocol -> Int
solveB (SafetyProtocol por ptp) =
  specialSum $ mapMaybe (`fix` por) (mapMaybe (`check2` por) ptp)

inputEx :: Text
inputEx = T.unlines
  [ "47|53"
  , "97|13"
  , "97|61"
  , "97|47"
  , "75|29"
  , "61|13"
  , "75|53"
  , "29|13"
  , "97|29"
  , "53|29"
  , "61|53"
  , "97|53"
  , "61|29"
  , "47|13"
  , "75|47"
  , "97|75"
  , "47|61"
  , "75|61"
  , "47|29"
  , "75|13"
  , "53|13"
  , ""
  , "75,47,61,53,29"
  , "97,61,53,29,13"
  , "75,29,13"
  , "75,97,47,61,53"
  , "61,13,29"
  , "97,13,75,29,47"
  ]

getIn :: IO SafetyProtocol
getIn = do
  i <- T.readFile "inputs/day5.input"
  pure $ fromRight (SafetyProtocol [] []) $ P.parseOnly parseInput i

main :: IO ()
main = do
  input <- T.readFile "inputs/day5.input"
  putStrLn "Part 1"
  let sp1 = fromRight (SafetyProtocol [] []) $ P.parseOnly parseInput input
  print $ solveA sp1
  putStrLn "Part 2"
  let sp2 = fromRight (SafetyProtocol [] []) $ P.parseOnly parseInput input
  print $ solveB sp2
