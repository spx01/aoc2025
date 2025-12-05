module Main (main) where

import qualified AOC
import Util

import qualified Data.Set as S
import Control.Arrow
import Data.List.Split
import Control.Monad
import Data.Functor
import Data.Maybe

type Range = Maybe (Int, Int)

parse :: String -> [Range]
parse = id
  >>> splitOn ","
  >>> fmap parseRange
  where
    parseRange s = case splitOn "-" s of
      [read -> l, read -> r]
        | l <= r    -> Just (l, r)
        | otherwise -> Nothing
      _             -> Nothing

part1 :: String -> String
part1 = parse >>> solve1 >>> concat >>> sum >>> show

part2 :: String -> String
part2 = parse >>> solve2 >>> fmap sum >>> sum >>> show

main :: IO ()
main = AOC.wrapInteract 2 (AOC.combineParts part1 part2)

solve1 :: [Range] -> [[Int]]
solve1 = fmap (invalids' 2)

extractRoot' :: Int -> Int -> Maybe Int
extractRoot' base i = do
  guard $ n `mod` base == 0
  guard $ all (== head' chunks) chunks
  pure $ read (head' chunks)
  where
    s      = show i
    n      = length s
    unit   = n `div` base
    chunks = chunksOf unit s

mkInvalid' :: Int -> Int -> Int
mkInvalid' base root = read . concatMap show $ replicate base root

invalids' :: Int -> Range -> [Int]
invalids' base = aux . simplifyRange' base
  where
    aux Nothing = []
    aux (Just (il, r)) = takeWhile (<= r) . fmap (mkInvalid' base) $
      iterate (+1) (fromJust $ extractRoot' base il)

simplifyRange' :: Int -> Range -> Range
simplifyRange' base ra@(Just (_, r))
  | Just l' <- firstInvalid' base ra = Just (l', r)
simplifyRange' _ _ = Nothing

firstInvalid' :: Int -> Range -> Maybe Int
firstInvalid' base = (>>= aux)
  where
    aux (l, r) = listToMaybe . mapMaybe go $ [l .. r]
    go i = extractRoot' base i $> i

solve2 :: [Range] -> [S.Set Int]
solve2 = fmap go
  where
    go r = S.fromList $ concatMap (`invalids'` r) [2 .. 12]

{-
>>> solve2 (parse "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")
[fromList [11,22],fromList [99,111],fromList [999,1010],fromList [1188511885],fromList [222222],fromList [],fromList [446446],fromList [38593859],fromList [565656],fromList [824824824],fromList [2121212121]]
-}
