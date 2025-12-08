module Main (main) where

import qualified AOC
import Util

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Coerce
import Control.Arrow
import Data.Foldable
import Linear.V3
import Linear.Metric
import Data.List.Split
import Data.List
import Data.Ord
import Control.Monad.ST
import Data.Maybe

data NodeT
type Node      = TInt NodeT
type Point     = V3 Int
type NodeTable = M.Map Point Node
type DSU s     = VM.STVector s (Node, Int)
type Edge      = (Point, Point)

nodeIx :: Point -> NodeTable -> Node
nodeIx = flip (M.!)

edgeIx :: Edge -> NodeTable -> (Node, Node)
edgeIx (p1, p2) = nodeIx p1 &&& nodeIx p2

numNodes :: NodeTable -> Int
numNodes = M.size

mkTable :: [Point] -> NodeTable
mkTable ps = M.fromList (zip ps [0 ..])

edges :: [Point] -> [Edge]
edges ps = sortOn (uncurry qd) [(p1, p2) | p1 <- ps, p2 <- ps, p1 < p2]

mkDSU :: Int -> ST s (DSU s)
mkDSU n = V.thaw . V.fromList $ [0 .. coerce n - 1] `zip` repeat 1

subSize :: Node -> DSU s -> ST s Int
subSize n d = snd <$> (d `VM.read` coerce n)

parent :: Node -> DSU s -> ST s Node
parent n d = fst <$> (d `VM.read` coerce n)

root :: Node -> DSU s -> ST s Node
root n d = do
  p <- parent n d
  if p == n
    then pure n
    else root p d

rootData :: DSU s -> ST s [(Node, Int)]
rootData = VM.ifoldr (\i p@(n, _) a -> if i == coerce n then p : a else a) []

compSizes :: DSU s -> ST s [Int]
compSizes = fmap (fmap snd) . rootData

connect :: forall s. Node -> Node -> DSU s -> ST s ()
connect u v d = do
  ru <- root u d
  rv <- root v d
  su <- subSize ru d
  sv <- subSize rv d
  case () of
    _ | ru == rv  -> pure ()
      | su < sv   -> aux ru su rv
      | otherwise -> aux rv sv ru
  where
    aux :: Node -> Int -> Node -> ST s ()
    aux c cs p = do
      VM.modify d (first (const p)) (coerce c)
      VM.modify d (second (+ cs))   (coerce p)

addEdge :: Edge -> NodeTable -> DSU s -> ST s ()
addEdge e nt = uncurry connect (edgeIx e nt)

parse :: String -> [Point]
parse = lines >>> fmap (splitOn ",") >>> fmap toPoint
  where
    toPoint :: [String] -> Point
    toPoint [x, y, z] = V3 (read x) (read y) (read z)
    toPoint _         = undefined

parse' :: String -> ([Edge], NodeTable)
parse' s = (es, nt)
  where
    ps = parse s
    nt = mkTable ps
    es = edges ps

solve1 :: [Edge] -> NodeTable -> [Int]
solve1 es nt = runST $ do
  d <- mkDSU (numNodes nt)
  forM_ (take 1000 es) $ \e -> addEdge e nt d
  sortOn Down <$> compSizes d

solve2 :: [Edge] -> NodeTable -> Edge
solve2 es0 nt = fromJust (runST $ mkDSU (numNodes nt) >>= aux es0 Nothing)
  where
    aux :: [Edge] -> Maybe Edge -> DSU s -> ST s (Maybe Edge)
    aux [] me _ = pure me
    aux (e:es) me d = do
      cs <- compSizes d
      case cs of
        [_] -> pure me
        _   -> do
          addEdge e nt d
          aux es (Just e) d

part1 :: String -> String
part1 = parse' >>> uncurry solve1 >>> take 3 >>> product >>> show

part2 :: String -> String
part2 = parse' >>> uncurry solve2 >>> extract >>> show
  where
    extract :: Edge -> Int
    extract (V3 x1 _ _, V3 x2 _ _) = x1 * x2

main :: IO ()
main = AOC.wrapInteract 8 (AOC.combineParts part1 part2)
