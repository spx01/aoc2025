module Main (main) where

import qualified AOC
import Util

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Arrow
import Data.List
import Data.Foldable
import Data.Ord
import Data.Int
import Data.Function

type Layer   = S.Set Int
type IxLayer = (Int, Layer)
type Pos     = (Int, Int)
type GridMap = M.Map Pos
data Dir     = R | L

data SplitTree
  = Null
  | Node !Int64 SplitTree SplitTree
  deriving (Show)

flipDir :: Dir -> Dir
flipDir L = R
flipDir R = L

flipNode :: SplitTree -> SplitTree
flipNode Null         = Null
flipNode (Node i l r) = Node i r l

count :: SplitTree -> Int64
count Null         = 1
count (Node i _ _) = i

addChild :: Dir -> SplitTree -> SplitTree -> SplitTree
addChild L c (Node _ _ r) = Node (count c + count r) c r
addChild R c p            = p & flipNode & addChild L c & flipNode
addChild _ _ Null         = undefined

terminal :: SplitTree
terminal = Node 2 Null Null

upAdj :: Dir -> Pos -> Pos -> Bool
upAdj d p0 p = fst p < fst p0 && snd p0 - snd p == dx
  where
    dx = case d of { L -> 1; R -> (-1) }

upAdjs :: Dir -> Pos -> [Pos] -> [Pos]
upAdjs d p0 = filter (upAdj d p0)

simulate :: Layer -> [Layer] -> [Pos]
simulate beams0 = snd . foldl' go (beams0, []) . zip [0 ..]
  where
    go :: FoldL (Layer, [Pos]) IxLayer
    go (beams, acc) (lix, splitters) = (beams', ((lix,) <$> hit) ++ acc)
      where
        hit      = toList $ S.intersection beams splitters
        newBeams = [subtract 1, (+ 1)] <*> hit
        beams'   = S.union (S.fromList newBeams) beams S.\\ S.fromList hit

mkSplitTree :: [Pos] -> SplitTree
mkSplitTree xs = aux m0 xs'
  where
    xs' = sortOn (Down . fst) xs
    m0  = M.fromList $ (, terminal) <$> xs'

    aux :: GridMap SplitTree -> [Pos] -> SplitTree
    aux m [root] = m M.! root
    aux m (p:ps) = aux m' ps
      where
        t       = m M.! p
        parents = [(dir, par) | dir <- [L, R], par <- upAdjs dir p ps]
        m'      = foldl' go m parents

        go :: FoldL (GridMap SplitTree) (Dir, Pos)
        go m (dir, neigh) = M.adjust (addChild (flipDir dir) t) neigh m

    aux _ _ = undefined

mkSL :: String -> Layer
mkSL = findIndices (/= '.') >>> S.fromList

parse :: String -> (Layer, [Layer])
parse = lines >>> fmap mkSL >>> uncons'

part1 :: String -> String
part1 = parse >>> uncurry simulate >>> length >>> show

part2 :: String -> String
part2 = parse >>> uncurry simulate >>> mkSplitTree >>> count >>> show

main :: IO ()
main = AOC.wrapInteract 7 (AOC.combineParts part1 part2)
