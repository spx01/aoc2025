module Main where

import qualified AOC
import Util

import Control.Arrow
import Data.Coerce

data MoveTag
type Move = TInt MoveTag

data PosTag
type Pos = TInt PosTag

pos0, posMod :: Pos
pos0 = 50
posMod = 100

applyMove :: Move -> Pos -> Pos
applyMove m p = (coerce m + p) `mod` posMod

turns :: Move -> Pos -> Int
turns m p
  | m < 0     = turns (-m) ((posMod - p) `mod` posMod)
  | otherwise = (coerce m + coerce p) `div` coerce posMod

applyMove' :: Move -> Pos -> (Int, Pos)
applyMove' m p = (turns m p, applyMove m p)

parseMove :: String -> Move
parseMove ('R':cs) = TInt $ read cs
parseMove ('L':cs) = TInt $ negate (read cs)
parseMove _        = undefined

part1 :: String -> String
part1 = show . solve1 . fmap parseMove . lines

part2 :: String -> String
part2 = show . solve2 . fmap parseMove . lines

main :: IO ()
main = AOC.wrapInteract 1 (AOC.combineParts part1 part2)

solve1 :: [Move] -> Int
solve1 = length . filter (== 0) . scanl (flip applyMove) pos0

solve2 :: [Move] -> Int
solve2 = fst . foldl' go (0, pos0)
  where
    go :: (Int, Pos) -> Move -> (Int, Pos)
    go (turns, p) m =
      first (turns +) $ applyMove' m p
