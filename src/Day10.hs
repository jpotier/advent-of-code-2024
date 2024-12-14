{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Day10 where

import qualified Algebra.Graph as D
import qualified Algebra.Graph.AdjacencyMap as A
import qualified Algebra.Graph.AdjacencyMap.Algorithm as A
import qualified Algebra.Graph.Class as C
import Algebra.Graph.Export
import Algebra.Graph.Export.Dot (exportViaShow)
import Algebra.Graph.ToGraph
import qualified Algebra.Graph.Undirected as U
import Data.Attoparsec.Text (Parser, digit, many1', parseOnly, sepBy, space)
import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import qualified Data.Foldable as F
import Data.Functor.Base (TreeF (..))
import Data.Functor.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Traversable as T
import qualified Data.Vector as V
import Debug.Trace
import GHC.Generics (Generic, Generic1)

type Pos = Int
type Height = Int
type Score = Sum Int
type NodeV = (Pos, Height)
type NodeVS = (Pos, Height, Score)

-- Recursion scheme thingies (why not?)
-- Isomorphic to Algebra.Graph.Graph
data GraF a b
  = EmptyF
  | VertexF a
  | -- (V1, E1) + (V2, E2) = (V1 ∪ V2, E1 ∪ E2)
    OverlayF b b
  | -- (V1, E1) × (V2, E2) = (V1 ∪ V2, E1 ∪ E2 ∪ V1 × V2)
    ConnectF b b
  deriving
    (Eq, Ord, Show, Read, Generic, Generic1, Functor, F.Foldable, T.Traversable)
type instance Base (D.Graph a) = GraF a
instance Recursive (D.Graph a) where
  project D.Empty = EmptyF
  project (D.Vertex v) = VertexF v
  project (D.Overlay g1 g2) = OverlayF g1 g2
  project (D.Connect g1 g2) = ConnectF g1 g2

instance Corecursive (D.Graph a) where
  embed EmptyF = D.Empty
  embed (VertexF v) = D.Vertex v
  embed (OverlayF g1 g2) = D.Overlay g1 g2
  embed (ConnectF g1 g2) = D.Connect g1 g2

-- Our own recursive Tree-like datatype
-- That type just happens to correspond to what adjacencyList returns!
newtype Treeish a = Treeish (a, [Treeish a])

-- I swear it's like a recursive Tree Functor
type instance Base (Treeish a) = TreeF a

-- Now the "hard" part
instance Recursive (Treeish a) where
  project (Treeish (x, xs)) = NodeF x xs

instance Corecursive (Treeish a) where
  embed (NodeF x xs) = Treeish (x, xs)

myShow :: (Show a) => D.Graph a -> String
myShow = para $ \case
  EmptyF -> "ε"
  VertexF v -> show v
  OverlayF (D.Vertex v1, _) (D.Vertex v2, _) ->
    "(" <> show v1 <> "+" <> show v2 <> ")"
  OverlayF (_, g1) (_, g2) -> "(" <> g1 <> "---" <> g2 <> ")"
  ConnectF (D.Vertex v1, _) (D.Vertex v2, _) ->
    "<" <> show v1 <> "×" <> show v2 <> ">"
  ConnectF (_, g1) (_, g2) -> "<" <> g1 <> " × " <> g2 <> ">"

legit :: D.Graph NodeV -> Bool
legit = para $ \case
  EmptyF -> False
  VertexF _v -> True
  OverlayF (_, r1) (_, r2) -> r1 && r2
  ConnectF (D.Vertex (_, h1), _) (D.Vertex (_, h2), _) -> h2 == h1 + 1
  ConnectF (_, r1) (_, r2) -> r1 && r2

countStuff :: D.Graph NodeV -> Sum Int
countStuff g = foldMap go $ S.filter (\(_, h) -> h == 0) $ D.vertexSet g
  where
    go :: NodeV -> Sum Int
    go v = Sum . length . filter (\(_, h) -> h == 9) $ reachable g v

-- Part B

countStuffB :: [(NodeV, [NodeV])] -> Int
countStuffB = extractSum . annotate10

annotate10 :: [(NodeV, [NodeV])] -> Map NodeV (Sum Int)
annotate10 xs =
  annotate 0 xs --  ^
    . annotate 1 xs --  |
    . annotate 2 xs --  | Walk back
    . annotate 3 xs --  | from height 9
    . annotate 4 xs --  | to height 0
    . annotate 5 xs --  |
    . annotate 6 xs --  | counting how many
    . annotate 7 xs --  | ways to get to 9
    . annotate 8 xs --  |
    . annotate 9 xs --  |
    $ mempty

annotate ::
  Height -> [(NodeV, [NodeV])] -> Map NodeV (Sum Int) -> Map NodeV (Sum Int)
annotate 9 xs _ =
  M.fromList . map ((,Sum 1) . fst) . filter ((== 9) . snd . fst) $ xs
annotate h xs acc =
  foldr go' acc . filter ((== h) . snd . fst) $ xs
  where
    go' :: (NodeV, [NodeV]) -> Map NodeV (Sum Int) -> Map NodeV (Sum Int)
    go' (v, nx) =
      M.insert v (F.fold $ M.restrictKeys acc (S.fromList nx))

extractSum :: Map NodeV (Sum Int) -> Int
extractSum = getSum . M.foldrWithKey go mempty
  where
    go :: NodeV -> Sum Int -> Sum Int -> Sum Int
    go (_, 0) s acc = s <> acc
    go _ _ acc = acc

-- From Algebra.Graph.*
showGraph ::
  (Show a, Ord a, C.Graph g, ToGraph g, ToVertex g ~ a) => g -> String
showGraph = render . export vDoc eDoc
  where
    vDoc x = literal (show x) <> "\n"
    eDoc x y = literal (show x) <> " - " <> literal (show y) <> "\n"

writeGraphOut ::
  (Show a, Ord a, C.Graph g, ToGraph g, ToVertex g ~ a) => g -> IO ()
writeGraphOut = writeFile "graph.dot" . exportViaShow

-- "Newton" neighboring topology were two points from a 2D grid are neighbors
-- if they are on the same line or column (up,down,left,right)
-- (the name comes from http://mgs.lacl.fr/Online_Manual/Collections.html)
-- Pos is the indexing order from "bottom" to "top", "left" to "right"
--
-- Example for newtonNeighboring 3:
--  6-7-8
--  | | |      "4" has 4 neighbors [1,3,7,5]
--  3-4-5  ->  "2" has 2 neighbors [1,5]
--  | | |      "7" has 3 neighbors [4,6,8]
--  0-1-2
newtonNeighboring :: Int -> U.Graph Pos
newtonNeighboring n =
  U.edges $
    concatMap
      ( filter (\(x, x') -> x >= 0 && x < n2 && x' >= 0 && x' < n2)
          . indexFromCoord
          . coordFromIndex
      )
      [0 .. n2 - 1]
  where
    n2 = n * n
    coordFromIndex :: Int -> [((Int, Int), (Int, Int))]
    coordFromIndex i =
      [ ((x, y), (x, y + 1)) -- up
      , ((x, y), (x, y - 1)) -- down
      , ((x, y), (x + 1, y)) -- right
      , ((x, y), (x - 1, y)) -- left
      ]
      where
        x = i `mod` n
        y = i `div` n
    indexFromCoord :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
    indexFromCoord = mapMaybe go
      where
        go (c1, c2) = do
          i1 <- c2i c1
          i2 <- c2i c2
          pure (i1, i2)
        c2i (x, y) | x >= 0 && x < n = Just $ y * n + x
        c2i _ = Nothing

parseInput :: Parser (Int, V.Vector Int)
parseInput = do
  p <- many1' parseDigit `sepBy` space
  let l = length . (!! 0) $ p
  let v = V.fromList . concat $ p
  pure (l, v)

parseDigit :: Parser Int
parseDigit = read . (: []) <$> digit

buildNewtonGraph :: Int -> V.Vector Int -> U.Graph (Pos, Int)
buildNewtonGraph n v = fmap setValue ng
  where
    ng = newtonNeighboring n
    setValue :: Pos -> (Pos, Int)
    setValue p = (p, v V.! p)

buildGraph :: T.Text -> D.Graph (Int, Int)
buildGraph txt = g'
  where
    (n, vals) =
      fromRight (0, V.empty) . parseOnly parseInput . T.unlines . reverse . T.lines $
        txt
    g = U.fromUndirected $ buildNewtonGraph n vals
    g' = D.edges (filter isHikingTrail $ D.edgeList g)
    isHikingTrail ((_, v1), (_, v2)) = v2 == v1 + 1

solveB :: (Show a) => a -> a
solveB = undefined

inputEx :: T.Text
inputEx =
  T.unlines
    [ "89010123"
    , "78121874"
    , "87430965"
    , "96549874"
    , "45678903"
    , "32019012"
    , "01329801"
    , "10456732"
    ]

otherInputEx :: T.Text
otherInputEx =
  T.unlines
    [ "1011911" -- 42 43 44 45 46 47 48
    , "2111811" -- 35 36 37 38 39 40 41
    , "3111711" -- 28 29 30 31 32 33 34
    , "4567654" -- 21 22 23 24 25 26 27
    , "1118113" -- 14 15 16 17 18 19 20
    , "1119112" -- 07 08 09 10 11 12 13
    , "1111101" -- 00 01 02 03 04 05 06
    ]

smallInputEx :: T.Text
smallInputEx =
  T.unlines
    [ "210" -- 06 07 08
    , "121" -- 03 04 05
    , "032" -- 00 01 02
    ]

main :: IO ()
main = do
  input <- T.readFile "inputs/day10.input"
  putStrLn "Part 1"
  let g = buildGraph input
  print . countStuff $ g
  putStrLn "Part 2"
  print . countStuffB $ D.adjacencyList g

-- print $ solveB input
