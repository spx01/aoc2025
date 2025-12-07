module Main (main) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Control.Arrow
import Data.Int
import Data.Foldable

main :: IO ()
main = interact (show . solve . parse)

parse :: String -> [S.Set Int]
parse = fmap (S.fromList . findIndices (/= '.')) . lines

solve :: [S.Set Int] -> (Int, Int64)
solve (l:ls) = let (p1, _, m) = foldl' go (0, l, m0) ls in (p1, sum m)
  where
    m0 = M.fromSet (const 1) l
    go (ns, b, m) s = (ns + S.size is, b', M.unionWith (+) md m1)
      where
        fs = [(+ 1), subtract 1]
        is = S.intersection s b
        nu = S.fromList $ fs <*> toList is
        b' = b S.\\ is `S.union` nu
        m1 = M.withoutKeys m is
        kv = (first <$> fs) <*> M.assocs (M.restrictKeys m is)
        md = M.fromListWith (+) kv
solve _ = undefined
