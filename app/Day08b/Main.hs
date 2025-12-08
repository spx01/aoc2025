module Main (main) where

import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Tree as T
import Linear.V3
import Linear.Metric
import Data.List.Split
import Data.Ord
import Data.List
import Data.Tuple
import Control.Arrow
import Data.Function
import Data.Maybe

type Point = V3 Double

type NodeTable = M.Map Point Int

nodeIx :: Point -> NodeTable -> Int
nodeIx = flip (M.!)

edgeIx :: (Point, Point) -> NodeTable -> (Int, Int)
edgeIx (p1, p2) = nodeIx p1 &&& nodeIx p2

numNodes :: NodeTable -> Int
numNodes = M.size

mkTable :: [Point] -> NodeTable
mkTable ps = M.fromList (zip ps [0 ..])

main :: IO ()
main = interact (show . (solve1 1000 &&& solve2) . parse)

parse :: String -> [Point]
parse = fmap (aux . splitOn ",") . lines
  where
    aux [a, b, c] = V3 (read a) (read b) (read c)
    aux _ = undefined

distinctPairs :: (Ord a) => [a] -> [(a, a)]
distinctPairs as = [(a1, a2) | a1 <- as, a2 <- as, a1 < a2]

compLengths :: [(Point, Point)] -> NodeTable -> [Int]
compLengths ps nt = cs & fmap (length . T.flatten)
  where
    n  = numNodes nt
    g  = G.buildG (0, n - 1) (ps >>= \p -> (`edgeIx` nt) <$> [p, swap p])
    cs = G.components g

solve1 :: Int -> [Point] -> Int
solve1 m xs = product . take 3 . sortOn Down $ compLengths ps nt
  where
    ps = take m . sortOn (uncurry distance) $ distinctPairs xs
    nt = mkTable xs

solve2 :: [Point] -> Int
solve2 xs = round x1 * round x2
  where
    nt = mkTable xs
    ps = sortOn (uncurry distance) $ distinctPairs xs
    ei = fromJust (go 1 (length ps) Nothing) - 1
    (V3 x1 _ _, V3 x2 _ _) = ps !! ei

    go :: Int -> Int -> Maybe Int -> Maybe Int
    go l r ans | l > r = ans
    go l r ans =
      case compLengths (take m ps) nt of
        [_] -> go l (m - 1) (Just m)
        _   -> go (m + 1) r ans
      where
        m = (l + r) `div` 2
