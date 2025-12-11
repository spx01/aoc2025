{-# LANGUAGE NoOverloadedStrings #-}

module Main (main) where

import qualified AOC
import Util

import qualified Data.Map as M
import qualified Data.Graph.Inductive as G
import Control.Lens
import Data.List.Split
import Data.List

type Vert  = Int
type Graph = G.Gr () ()

mapVerts :: [String] -> M.Map String Vert
mapVerts = foldl' go mempty
  where
    go m i = M.insertWith (\_new old -> old) i (M.size m) m

parseEdgeLine :: M.Map String Vert -> [String] -> [(Vert, Vert)]
parseEdgeLine m s = (m M.! src,) . (m M.!) <$> dsts
  where
    (src, dsts) = uncons' s

parse :: String -> (M.Map String Vert, Graph)
parse s = (vertMap, G.mkUGraph [0 .. n - 1] edges)
  where
    edgeLines = s & lines & fmap (wordsBy (`elem` ": "))
    allNames  = concat edgeLines
    vertMap   = mapVerts allNames
    edges     = edgeLines <&> parseEdgeLine vertMap & concat
    n         = M.size vertMap

countPaths :: Vert -> Vert -> Graph -> Int
countPaths start fin g = counts M.! start
  where
    counts = M.fromList [(n, go n) | n <- G.nodes g]
    go node
      | node == fin = 1
      | otherwise   = sum [counts M.! neigh | neigh <- G.suc g node]

countPaths' :: Vert -> Vert -> [Vert] -> Graph -> Int
countPaths' start fin vis g = sum (composePaths <$> paths)
  where
    options      = [start : p ++ [fin] | p <- permutations vis]
    paths        = [o `zip` drop 1 o | o <- options]
    composePaths = product . fmap (\(u, v) -> countPaths u v g)

part1 :: String -> String
part1 s = countPaths you out g & show
  where
    (m, g)     = parse s
    (you, out) = ("you", "out") & each %~ (m M.!)

part2 :: String -> String
part2 s = countPaths' svr out [dac, fft] g & show
  where
    (svr, out, dac, fft) = ("svr", "out", "dac", "fft") & each %~ (m M.!)
    (m, g)               = parse s

main :: IO ()
main = AOC.wrapInteract 11 (AOC.combineParts part1 part2)
