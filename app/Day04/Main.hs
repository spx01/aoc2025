module Main (main) where

import qualified AOC
import Util

import qualified Data.Set as S
import Data.Functor
import Control.Arrow
import Control.Monad
import Control.Monad.State.Strict

type Pos      = (Int, Int)
type Board    = S.Set Pos
type BoardS a = State (Pos, Board) a

boardSize :: BoardS Pos
boardSize = gets fst

boardSet :: BoardS Board
boardSet = gets snd

part1 :: String -> String
part1 = parse >>> evalState solve1 >>> length >>> show

part2 :: String -> String
part2 = parse >>> evalState solve2 >>> length >>> show

onBoard :: Pos -> BoardS Bool
onBoard p = gets $ S.member p . snd

rmPos :: Pos -> BoardS ()
rmPos = modify . second . S.delete

parse :: String -> (Pos, Board)
parse = lines >>> ((length &&& (head' >>> length)) &&& aux)
  where
    aux :: [String] -> Board
    aux = id
      >>> fmap (zip [0 :: Int ..] >>> filter (\(_, c) -> c == '@'))
      >>> fmap (fmap fst)
      >>> zipWith (fmap . (,)) [0 :: Int ..]
      >>> concat
      >>> S.fromList

adj :: Pos -> BoardS [Pos]
adj p@(x, y) = do
  (sx, sy) <- boardSize
  pure $
    [ (x', y')
    | x' <- [x - 1 .. x + 1], 0 <= x', x' < sx
    , y' <- [y - 1 .. y + 1], 0 <= y', y' < sy
    , (x', y') /= p
    ]

spaced :: Pos -> BoardS Bool
spaced p = do
  b  <- boardSet
  ns <- adj p
  let nsOcc = filter (`S.member` b) ns
  pure $ length (take 4 nsOcc) < 4

solve1 :: BoardS [Pos]
solve1 = boardSet >>= (S.toList >>> filterM spaced)

solve2 :: BoardS [Pos]
solve2 = gets snd >>= filterM spaced . S.toList >>= \sp0 -> do
  forM_ sp0 rmPos
  go sp0
  where
    go :: [Pos] -> BoardS [Pos]
    go []     = pure []
    go (r:rs) = do
      ns <- adj r >>= filterM (\x -> do
        ifM ((&&) <$> onBoard x <*> spaced x)
          (rmPos x $> True)
          (pure False))
      (r :) <$> go (ns ++ rs)

main :: IO ()
main = AOC.wrapInteract 4 (AOC.combineParts part1 part2)
