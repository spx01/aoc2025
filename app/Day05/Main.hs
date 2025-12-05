module Main (main) where

import qualified AOC
import Util

import Control.Arrow
import Data.Int
import Data.List
import Data.Maybe

type ID    = Int64
type Range = (ID, ID)

main :: IO ()
main = AOC.wrapInteract 5 (AOC.combineParts part1 part2)

accepts :: ID -> Range -> Bool
accepts i (l, r) = i >= l && i <= r

rangeSize :: Range -> Int64
rangeSize (l, r) = r - l + 1

part1 :: String -> String
part1 = parse >>> solve1 >>> length >>> show

part2 :: String -> String
part2 = parse >>> fst >>> solve2 >>> show

parse :: String -> ([Range], [ID])
parse = id
  >>> lines
  >>> breakDrop (== "")
  >>> (fmap parseRange *** fmap read)

parseRange :: String -> Range
parseRange = id
  >>> breakDrop (== '-')
  >>> (read *** read)

solve1 :: ([Range], [ID]) -> [ID]
solve1 (rs, ids) = filter (flip any rs . accepts) ids

normalizeRanges :: [Range] -> [Range]
normalizeRanges = sort >>> mapAccumL go (-1) >>> snd >>> catMaybes
  where
    go :: Int64 -> Range -> (Int64, Maybe Range)
    go m (l, r) =
      if l' <= r
        then (r, Just (l', r))
        else (m, Nothing)
      where
        l' = max (m + 1) l

solve2 :: [Range] -> Int64
solve2 = normalizeRanges >>> fmap rangeSize >>> sum
