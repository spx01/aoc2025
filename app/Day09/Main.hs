module Main (main) where

import AOC
import Util

import Linear.V2
import Data.List.Split
import Control.Arrow

type Pos  = V2 Int
type Edge = (Pos, Pos)
type Rect = (Pos, Pos)

x, y :: Pos -> Int
x (V2 x _) = x; y (V2 _ y) = y

rx0, ry0, rx1, ry1 :: Rect -> Int
rx0 = x . fst; ry0 = y . fst; rx1 = x . snd; ry1 = y . snd

parse :: String -> [Pos]
parse = lines >>> fmap (splitOn ",") >>> fmap toPos
  where
    toPos :: [String] -> Pos
    toPos (fmap read -> [x, y]) = V2 x y
    toPos _ = undefined

edges :: [Pos] -> [Edge]
edges ps = ps `zip` (drop 1 ps ++ [head' ps])

mkRect :: Pos -> Pos -> Rect
mkRect (V2 x1 y1) (V2 x2 y2) =
  (V2 (min x1 x2) (min y1 y2), V2 (max x1 x2) (max y1 y2))

mids :: Rect -> [Pos]
mids r = uncurry V2 <$> (liftA2 (***) [id, (+1)] [id, (+1)] <*> [(mx, my)])
  where
    mx = (rx0 r + rx1 r) `div` 2
    my = (ry0 r + ry1 r) `div` 2

insidePoly :: Pos -> [Edge] -> Bool
insidePoly (V2 px py) = odd . length . filter crosses
  where
    crosses (V2 x1 y1, V2 x2 y2)
      | y1 == y2  = y1 > py && min x1 x2 <= px && px < max x1 x2
      | otherwise = False

edgeCrossesRect :: Rect -> Edge -> Bool
edgeCrossesRect r (V2 x1 y1, V2 x2 y2)
  | x1 == x2  =
    rx0 r < x1 && x1 < rx1 r
    && min y1 y2 < ry1 r
    && ry0 r < max y1 y2
  | y1 == y2  =
    ry0 r < y1 && y1 < ry1 r
    && min x1 x2 < rx1 r
    && rx0 r < max x1 x2
  | otherwise = False

validRect :: [Edge] -> Rect -> Bool
validRect es r = 
  not (any (edgeCrossesRect r) es)
  && all (`insidePoly` es) (mids r)

area :: Rect -> Int
area r = (abs (rx1 r - rx0 r) + 1) * (abs (ry1 r - ry0 r) + 1)

part1 :: String -> String
part1 s = show . maximum $
  [ area r
  | u <- ps, v <- ps, u < v
  , let r = mkRect u v
  ]
  where
    ps = parse s

part2 :: String -> String
part2 s = show . maximum  $
  [ area r
  | u <- ps, v <- ps, u < v
  , let r = mkRect u v
  , validRect es r
  ]
  where
    ps = parse s
    es = edges ps

main :: IO ()
main = AOC.wrapInteract 9 (AOC.combineParts part1 part2)
