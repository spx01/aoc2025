-- | Non-working alternative for part 2

{-# LANGUAGE PatternSynonyms #-}

module Main where

import Util

import qualified Data.Sequence as S
import qualified Data.Set as SE
import Data.Set (Set)
import Data.Sequence (Seq(..))
import Linear.V2
import Control.Lens
import Data.Coerce
import Control.Arrow
import Data.Foldable
import Data.List.Split
import Data.List
import Data.Ord

type Pos = V2 Int
type Vx  = (Wind, Pos)

-- | A CCW loop where the first vertex is to the left of the next.
type Loop = Seq Vx

newtype Wind = Wind Int
  deriving newtype (Ord, Eq, Num, Enum, Real, Integral, Show)

newtype Dir = Dir Int
  deriving newtype (Ord, Eq, Show)

pattern R :: Dir; pattern R = Dir 0
pattern U :: Dir; pattern U = Dir 1
pattern L :: Dir; pattern L = Dir 2
pattern D :: Dir; pattern D = Dir 3

reflectX :: Pos -> Pos; reflectX = _x %~ negate
windOf :: Dir -> Wind; windOf (Dir d) = Wind $ if d == 3 then (-1) else d
dirOf :: Wind -> Dir; dirOf = Dir . (`mod` 4) . coerce
rotCCW :: Pos -> Pos; rotCCW (V2 x y) = V2 (-y) x
reversePos :: Pos -> Pos; reversePos = reflectX . rotCCW
reverseVx :: Vx -> Vx; reverseVx = _2 %~ reversePos
wnd3 :: Seq a -> Seq (a, a, a); wnd3 s = S.zip3 (rotL (-1) s) s (rotL 1 s)
crossWind :: Pos -> Pos -> Wind; crossWind p1 p2 = Wind . signum $ crossZ p1 p2
rotVx :: Wind -> Vx -> Vx; rotVx w = _2 %~ rotPos w
sortRect :: Endo' (Vx, Vx); sortRect (v1, v2) = (min v1 v2, max v1 v2)
rotPos :: Wind -> Pos -> Pos
rotPos w p = foldr ($) p (replicate (coerce (dirOf w)) rotCCW)
reverseLoop :: Loop -> Loop
reverseLoop = S.reverse >>> rotL (-1) >>> fmap reverseVx
area :: (Vx, Vx) -> Int
area ((_, V2 x1 y1), (_, V2 x2 y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
parsePos :: String -> Pos
parsePos = splitOn ","
  >>> \case { (fmap read -> [x, y]) -> V2 x y; _ -> undefined }
dirBetween :: Pos -> Pos -> Dir
dirBetween p1@(V2 x1 y1) p2@(V2 x2 y2)
  | p1 == p2  = undefined
  | x1 == x2  = if y1 < y2 then U else D
  | y1 == y2  = if x1 < x2 then R else L
  | otherwise = undefined

mkLoop :: [Pos] -> Loop
mkLoop lps =
  case () of
    _ | wnd == (-4) -> mkLoop (reversePos <$> lps)
      | wnd == 4    -> vxs <&> rotVx (-w0)
      | otherwise   -> undefined
  where
    ws  = wnd3 (S.fromList lps)
    vxs = fmap (\(l, c, r) -> (crossWind (c - l) (r - l), c)) ws
    wnd = sum (fst <$> vxs)
    w0  = windOf $ (\(_, c, r) -> dirBetween c r) (shead' ws)

scanCCW :: forall a. (Ord a) => (Vx -> a) -> Loop -> Set a
scanCCW f ((_, V2 x0 y0) :<| rest) =
  foldl' go (0, Nothing, mempty) rest ^. _3
  where
    go :: FoldL (Wind, Maybe Int, Set a) Vx
    go acc@(!w, !h, !l) v@(deltaW, V2 x y)
      | x < x0 || y < y0     = acc'
      | Just xh <- h, x > xh = acc'
      | w < 0 && w' /= 0     = acc'
      | otherwise =
        let
          h' = case dirOf w' of
            R | y > y0 -> Just x
            _          -> Nothing
        in (w', h', f v `SE.insert` l)
      where
        w' = w + deltaW
        acc' = acc & _1 .~ w'
scanCCW _ _ = mempty

scanCW :: (Ord a) => (Vx -> a) -> Loop -> Set a
scanCW f = scanCCW (f . reverseVx) . reverseLoop

scanBoth :: (Ord a) => (Vx -> a) -> Loop -> Set a
scanBoth f lo = scanCCW f lo `SE.intersection` scanCW f lo

advance :: Loop -> (Wind, Loop)
advance lo = (w, rotVx (-w) <$> lo')
  where
    lo'           = rotL 1 lo
    (_, v0', v1') = shead' (wnd3 lo')
    w             = windOf (dirBetween (snd v0') (snd v1'))

scanAll :: Loop -> [(Vx, Vx)]
scanAll s = (iterate go s0 !! length s) ^. _1 & toList
  where
    s0 = (mempty, s, 0)

    go :: Endo' (Set (Vx, Vx), Loop, Wind)
    go (!acc, loopView@(f :<| _), !windBack) = (acc', loopView', windBack + dw)
      where
        fReal      = rotVx windBack f
        scanResult = scanBoth (sortRect . (fReal,) . rotVx windBack) loopView
        acc'       = scanResult `SE.union` acc
        (dw, loopView') = advance loopView
    go _ = undefined

main :: IO ()
main = do
  s <- getContents
  let loop = mkLoop (parsePos <$> lines s)
  let res = scanAll loop
  print (sortOn Down ((area &&& id) <$> res) & take 10)

convert = fmap (uncurry V2)
maze = mkLoop $ convert [(2, 4), (4, 4), (4, 2), (6, 2), (6, 3), (5, 3), (5, 4), (7, 4), (7, 1), (4, 1), (4, 0), (9, 0), (9, 4), (11, 4), (11, 6), (7, 6), (7, 8), (3, 8), (3, 6), (2, 6)]
advance' :: Loop -> Loop
advance' = snd . advance

l2 :: Loop
l2 = iterate advance' (S.fromList [(1,V2 7 1),(1,V2 11 1),(1,V2 11 7),(1,V2 9 7),(-1,V2 9 5),(1,V2 2 5),(1,V2 2 3),(-1,V2 7 3)]) !! 6

{-
>>> maze
fromList [(1,V2 2 4),(-1,V2 4 4),(1,V2 4 2),(1,V2 6 2),(1,V2 6 3),(-1,V2 5 3),(-1,V2 5 4),(-1,V2 7 4),(-1,V2 7 1),(1,V2 4 1),(1,V2 4 0),(1,V2 9 0),(-1,V2 9 4),(1,V2 11 4),(1,V2 11 6),(-1,V2 7 6),(1,V2 7 8),(1,V2 3 8),(-1,V2 3 6),(1,V2 2 6)]

>>> advance (advance' maze)
(1,fromList [(1,V2 4 2),(1,V2 6 2),(1,V2 6 3),(-1,V2 5 3),(-1,V2 5 4),(-1,V2 7 4),(-1,V2 7 1),(1,V2 4 1),(1,V2 4 0),(1,V2 9 0),(-1,V2 9 4),(1,V2 11 4),(1,V2 11 6),(-1,V2 7 6),(1,V2 7 8),(1,V2 3 8),(-1,V2 3 6),(1,V2 2 6),(1,V2 2 4),(-1,V2 4 4)])

>>> fmap (reverseVx) $ reverseLoop l2
fromList [(1,V2 2 3),(1,V2 2 5),(-1,V2 9 5),(1,V2 9 7),(1,V2 11 7),(1,V2 11 1),(1,V2 7 1),(-1,V2 7 3)]

>>> l2
fromList [(1,V2 2 3),(-1,V2 7 3),(1,V2 7 1),(1,V2 11 1),(1,V2 11 7),(1,V2 9 7),(-1,V2 9 5),(1,V2 2 5)]

>>> scanCCW id l2
fromList [(-1,V2 7 3),(-1,V2 9 5),(1,V2 2 5),(1,V2 9 7),(1,V2 11 7)]

>>> scanCW id l2
fromList [(-1,V2 7 3),(-1,V2 9 5),(1,V2 2 5)]

>>> dirBetween (V2 (-9) (-7)) (V2 (-9) (-5))
1
-}
