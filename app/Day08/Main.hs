module Main (main) where

import qualified AOC
import Util

import qualified Data.Sequence as S
import qualified Data.Map as M
import Data.Coerce
import Control.Arrow
import Data.Foldable
import Linear.V3
import Linear.Metric
import Data.List.Split
import Data.List
import Data.Ord

data NodeT
type Node      = TInt NodeT
type Point     = V3 Double
type NodeTable = M.Map Point Node
type DSU       = S.Seq (Node, Int)

mkDSU :: Int -> DSU
mkDSU n = S.fromList $ zip [0 .. coerce n - 1] (repeat 1)

subSize :: Node -> DSU -> Int
subSize n d = snd $ d `S.index` coerce n

getParent :: Node -> DSU -> Node
getParent n d = fst $ d `S.index` coerce n

getRoot :: Node -> DSU -> Node
getRoot n d = if p == n then n else getRoot p d
  where
    p = getParent n d

compSizes :: DSU -> [Int]
compSizes = filterWithIndex (\i (n, _) -> i == coerce n) >>> fmap snd >>> toList

connect :: Node -> Node -> DSU -> DSU
connect u v d =
  if ru == rv
    then d
    else
      if su < sv
        then aux ru su rv d
        else aux rv sv ru d
  where
    ru = getRoot u d
    rv = getRoot v d
    su = subSize ru d
    sv = subSize rv d

    aux :: Node -> Int -> Node -> DSU -> DSU
    aux c sc p = id
      >>> S.adjust' (first (const p)) (coerce c)
      >>> S.adjust' (second (+ sc)) (coerce p)

nodeIx :: Point -> NodeTable -> Node
nodeIx = flip (M.!)

edgeIx :: (Point, Point) -> NodeTable -> (Node, Node)
edgeIx (p1, p2) = nodeIx p1 &&& nodeIx p2

numNodes :: NodeTable -> Int
numNodes = M.size

mkTable :: [Point] -> NodeTable
mkTable ps = M.fromList (zip ps [0 ..])

parse :: String -> [Point]
parse = lines >>> fmap (splitOn ",") >>> fmap toPoint
  where
    toPoint :: [String] -> Point
    toPoint [x, y, z] = V3 (read x) (read y) (read z)
    toPoint _         = undefined

ordPairs :: (Ord a) => [a] -> [(a, a)]
ordPairs as = [(a1, a2) | a1 <- as, a2 <- as, a1 < a2]

pointPairs :: [Point] -> [(Point, Point)]
pointPairs = ordPairs >>> sortOn (uncurry distance)

addEdge :: (Point, Point) -> NodeTable -> DSU -> DSU
addEdge e nt = uncurry connect (edgeIx e nt)

scanDSUs :: NodeTable -> [(Point, Point)] -> [(DSU, (Point, Point))]
scanDSUs nt es = scanr (flip addEdge nt) (mkDSU n) res `zip` res
  where
    n   = numNodes nt
    res = reverse es

solve1 :: [Point] -> [(Point, Point)] -> [Int]
solve1 ps = fst . head' . scanDSUs nt >>> compSizes
  where
    nt = mkTable ps

solve2 :: [Point] -> [(Point, Point)] -> (Point, Point)
solve2 ps = scanDSUs nt >>> span (oneComp . fst) >>> snd . last . fst
  where
    nt = mkTable ps
    oneComp :: DSU -> Bool
    oneComp d = case compSizes d of [_] -> True; _ -> False

part1 :: String -> String
part1 = parse
  >>> (id &&& take 1000 . pointPairs)
  >>> take 3 . sortOn Down . uncurry solve1
  >>> product
  >>> show

part2 :: String -> String
part2 = parse
  >>> (id &&& pointPairs)
  >>> uncurry solve2
  >>> (\((V3 x1 _ _), (V3 x2 _ _)) -> round x1 * round x2 :: Int)
  >>> show

main :: IO ()
main = AOC.wrapInteract 8 (AOC.combineParts part1 part2)
