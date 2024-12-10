{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Day6 where

import Control.Parallel.Strategies
import Data.List (intersperse, unfoldr, sort)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Debug.Trace
import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.HashSet as H
-- import UnliftIO (mapConcurrently)

data MapObjects = MOEmpty | MOObstacle | MOExtraObstacle
                | MOGuard GuardDirection
  deriving Eq

data GuardDirection = GDNorth | GDEast | GDSouth | GDWest
  deriving (Show,Eq,Generic)

instance Hashable GuardDirection

s :: GuardDirection -> GuardDirection
s GDNorth  = GDEast
s GDEast   = GDSouth
s GDSouth  = GDWest
s GDWest   = GDNorth

instance Show MapObjects where
  show MOEmpty             = "."
  show MOObstacle          = "#"
  show MOExtraObstacle     = "O"
  show (MOGuard GDNorth)   = "^"
  show (MOGuard GDEast)    = ">"
  show (MOGuard GDSouth)   = "v"
  show (MOGuard GDWest)    = "<"

prettyLine :: V.Vector MapObjects -> String
prettyLine = intersperse ' ' . fmap (head . show) . reverse . V.toList

prettyMap :: AoCMap -> String
prettyMap (AoCMap (xmax,ymax) v) = go (xmax,ymax) (V.reverse v)
  where
    go (_,0) _ = ""
    go (xmax',ymax') v' = prettyLine cur <> "\n" <> go (xmax',ymax'-1) next
      where
        (cur,next) = V.splitAt xmax' v'

data AoCMap = AoCMap { getBounds :: Bounds, getObjects :: V.Vector MapObjects }
  deriving Show

type Bounds = (Int,Int) -- max x, max y

type Pos = (Int,Int)

toObjs :: Char -> MapObjects
toObjs '#' = MOObstacle
toObjs 'O' = MOExtraObstacle
toObjs '^' = MOGuard GDNorth
toObjs '>' = MOGuard GDEast
toObjs 'v' = MOGuard GDSouth
toObjs '<' = MOGuard GDWest
toObjs _ = MOEmpty

isMOGuard :: MapObjects -> Bool
isMOGuard (MOGuard _) = True
isMOGuard _ = False

guardDirection :: MapObjects -> Maybe GuardDirection
guardDirection (MOGuard x) = Just x
guardDirection _ = Nothing

parseInput :: Text -> AoCMap
parseInput txt = AoCMap (xmax,ymax) objs
  where
    xxs = T.lines txt
    xmax = T.length $ head xxs
    ymax = length xxs
    objs :: V.Vector MapObjects
    objs = V.fromList $ concatMap (map toObjs . T.unpack) (reverse xxs)

toIndex :: Pos -> Bounds -> Maybe Int
toIndex (x,y) (xmax,ymax)
  | x >= 0 && x < xmax && y >= 0 && y < ymax
  = Just $ x + y*xmax
toIndex _ _ = Nothing

toPos :: Int -> Int -> Pos
toPos i xmax = (i `mod` xmax, i `div` xmax)

guardPath :: AoCMap -> [Pos]
guardPath (AoCMap bounds@(xmax,_ymax) myMap) =
  unfoldr step (initialPos myMap xmax,initialOri myMap)
  where
    step :: (Pos,GuardDirection) -> Maybe (Pos, (Pos,GuardDirection))
    step (p,dir) = do
      i <- toIndex (nextPos dir p) bounds
      ahead <- myMap V.!? i
      pure $ case ahead of
        MOObstacle -> (p,(p,s dir))
        _          -> (p,(nextPos dir p,dir))

-- trace the trajectory of the Guard until it exits the map
-- return the number of unique positions it occupied, including the starting
-- one
solveA :: Text -> Int
solveA = (+1) . length . map NE.head . NE.group . sort . guardPath . parseInput

nextPos :: GuardDirection -> Pos -> Pos
nextPos GDNorth (x,y) = (x,y+1)
nextPos GDEast  (x,y) = (x+1,y)
nextPos GDSouth (x,y) = (x,y-1)
nextPos GDWest  (x,y) = (x-1,y)

initialPos :: V.Vector MapObjects -> Int -> Pos
initialPos myMap xmax = (`toPos` xmax) . fromJust $ V.findIndex isMOGuard myMap

initialOri :: V.Vector MapObjects -> GuardDirection
initialOri myMap = fromJust $ do
  guard <- V.find isMOGuard myMap
  guardDirection guard

type Trail = H.HashSet (Pos,GuardDirection)

isGuardCycle :: AoCMap -> Pos -> Bool
isGuardCycle (AoCMap bounds@(xmax,_ymax) myMap) newObs =
  traceShow newObs $
  or $ unfoldr step (initialPos myMap xmax
                    ,initialOri myMap
                    ,H.empty
                    ,False)
  where
    step :: (Pos,GuardDirection,Trail,Bool)
         -> Maybe (Bool, (Pos,GuardDirection,Trail,Bool))
    step (p,dir,trail,end) = do
      i <- toIndex (nextPos dir p) bounds
      ahead <- myMap V.!? i
      let isNewObs = nextPos dir p == newObs
      let nextTrail = H.insert (p,dir) trail
      case (ahead, isNewObs, (p,dir) `H.member` trail,end) of
        (_,_,_,True)        -> Nothing
        (_,_,True,_)        -> trace ">>>>>>>>>>>Cycle!<<<<<<<<<<<"
                               $ Just (True,(p,dir,nextTrail,True))
        (_,True,_,_)        -> Just (False,(p,s dir,nextTrail,False))
        (MOObstacle,_,_,_)  -> Just (False,(p,s dir,nextTrail,False))
        _                   -> Just (False,(nextPos dir p,dir,nextTrail,False))

freePos :: AoCMap -> [Pos]
freePos (AoCMap (xmax,_ymax) myMap) =
  V.toList ((`toPos` xmax) <$> V.elemIndices MOEmpty myMap)

solveB :: Text -> Int
solveB input = length . filter id
  $ parMap rpar (isGuardCycle aocmap) (freePos aocmap)
  where
    aocmap = parseInput input

-- solveB :: Text -> IO Int
-- solveB input = length . filter id
--   <$> mapConcurrently (pure . isGuardCycle aocmap) (freePos aocmap)
--   where
--     aocmap = parseInput input

inputEx :: Text
inputEx = T.unlines
  ["....#....."
  ,".........#"
  ,".........."
  ,"..#......."
  ,".......#.."
  ,".........."
  ,".#..^....."
  ,"........#."
  ,"#........."
  ,"......#..."
  ]

main :: IO ()
main = do
  input <- T.readFile "inputs/day6.input"
  putStrLn "Part 1"
  print $ solveA input
  putStrLn "Part 2"
  print $ solveB input
  --print =<< solveB input
