module Main where

import qualified Data.Sequence as S
import Data.Char
import Control.Arrow
import Data.Coerce
import Data.Monoid
import Data.Maybe
import Data.Foldable
import Data.Int

import qualified AOC

parse :: String -> [[Int]]
parse = fmap (fmap digitToInt) . lines

unconsMax :: (Ord a) => S.Seq a -> Maybe (a, S.Seq a)
unconsMax S.Empty = Nothing
unconsMax xs      = Just (m, S.drop (i + 1) xs)
  where
    m = maximum xs
    i = fromJust $ S.elemIndexL m xs

splitLast :: Int -> S.Seq a -> (S.Seq a, S.Seq a)
splitLast n s = S.splitAt (S.length s - n) s

takeNMax :: Int -> S.Seq Int -> S.Seq Int
takeNMax n0 = S.unfoldr go . (n0,)
  where
    go :: (Int, S.Seq Int) -> Maybe (Int, (Int, S.Seq Int))
    go (0, _) = Nothing
    go (n, s) =
      case unconsMax s1 of
        Just (m, r) -> Just (m, (n - 1, r S.>< s2))
        Nothing     -> Nothing
      where
        (s1, s2) = splitLast (n - 1) s

bankMax' :: Int -> [Int] -> Int64
bankMax' n = S.fromList >>> takeNMax n >>> fmap intToDigit >>> toList >>> read

solve :: Int -> String -> String
solve nm = parse >>> foldMap (Sum . bankMax' nm) >>> coerce >>> show @Int64

main :: IO ()
main = AOC.wrapInteract 3 (AOC.combineParts (solve 2) (solve 12))
