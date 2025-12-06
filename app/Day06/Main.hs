module Main (main) where

import qualified AOC

import Control.Arrow
import Data.List
import Data.Int
import Data.Maybe
import Data.List.Split

parse' :: String -> (String, [String])
parse' = id
  >>> lines
  >>> reverse
  >>> fromJust . uncons
  >>> second reverse

parse1 :: String -> [(Char, [Int64])]
parse1 = id
  >>> parse'
  >>> ((words >>> concat) *** (fmap words >>> transpose >>> fmap (fmap read)))
  >>> uncurry zip

parse2 :: String -> [(Char, [Int64])]
parse2 = id
  >>> parse'
  >>> ((words >>> concat) *** aux)
  >>> uncurry zip
  where
    aux :: [String] -> [[Int64]]
    aux = id
      >>> transpose
      >>> wordsBy (all (== ' '))
      >>> fmap (fmap read)

reduce :: (Num a) => Char -> [a] -> a
reduce '+' = sum
reduce '*' = product
reduce _   = undefined

part1 :: String -> String
part1 = parse1 >>> fmap (uncurry reduce) >>> sum >>> show

part2 :: String -> String
part2 = parse2 >>> fmap (uncurry reduce) >>> sum >>> show

main :: IO ()
main = AOC.wrapInteract 6 (AOC.combineParts part1 part2)
