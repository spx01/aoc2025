{-# LANGUAGE NoOverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Util
import qualified AOC

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Data.Vector as V
import Control.Arrow
import Data.Bits
import Data.Maybe
import Data.Void
import Control.Monad
import Data.List
import Data.Tuple
import Control.Lens
import Data.Foldable
import Data.Ord

pieceSize, dimMax :: Int
pieceSize = 3
dimMax    = 50

type Vector = V.Vector
type Parser = M.Parsec Void String
type Pos    = (Int, Int)
type Pie    = [Piece]
type Piece  = [Pos]
type Board  = (Pos, [Int])

type Occ     = Integer
type Piece'  = (Occ, Pos)
type Pie'    = [Piece']
type Constr  = (Int, Pie')
type Task    = Vector Constr

pnum :: Parser Int; pnum = read <$> M.some M.digitChar
plit :: String -> Parser (); plit = void . M.chunk

pieceBits :: Piece -> [Int]
pieceBits p = [j * dimMax + i | (i, j) <- p]

rotCCW :: Piece -> Piece
rotCCW p = [(-y + pieceSize, x) | (x, y) <- p]

pieceOcc :: Pos -> Piece -> Occ
pieceOcc placement p = fromBitIxs $ pieceBits $ translatePiece placement p

translatePiece :: Pos -> Piece -> Piece
translatePiece (i, j) = map (bimap (+ i) (+ j))

mkPie' :: Pos -> Piece -> Pie'
mkPie' dim p =
  [ (pieceOcc anchor orient, anchor)
  | anchor <- placements dim
  , orient <- mkPie p ]

ppiece :: Parser (Int, Piece)
ppiece = do
  i  <- pnum <* plit ":\n"
  ls <- M.some pline
  M.space
  pure (i, toPiece ls)
  where
    pline = M.takeWhile1P Nothing (`elem` ".#") <* plit "\n"
    toPiece ls = [(i, j) | (i, l) <- [0..] `zip` ls, (j, '#') <- [0..] `zip` l]

pboard :: Parser Board
pboard = do
  dim <- liftA2 (,) (pnum <* plit "x") (pnum <* plit ":")
  is  <- M.some (M.hspace *> pnum)
  M.space
  pure (dim, is)

parser1 :: Parser ([(Int, Piece)], [Board])
parser1 = liftA2 (,) (M.some (M.try ppiece)) (M.some pboard)

mkPie :: Piece -> Pie
mkPie p0 = nub $ sort <$> (rots p0 ++ rots (swap <$> p0))
  where rots p = take 4 $ drop 1 $ iterate rotCCW p

placements :: Pos -> [Pos]
placements (h, w) =
  sortOn (uncurry (+)) $ liftA2 (,) [0..h - pieceSize] [0..w - pieceSize]

solveBoard :: [(Int, Pie)] -> Board -> Bool
solveBoard pisIndexed (dim, pieceIdxs) = solve dim task
  where
    pis  = snd <$> sortOn fst pisIndexed
    pis' = concatMap (mkPie' dim) <$> pis
    task = V.fromList $ zip pieceIdxs pis'

solve :: Pos -> Task -> Bool
solve dim t0 = fitCheck && go t0 bounds0 0 /= []
  where
    totalFilled = sum [popCount occ * k | (k, head' -> (occ, _)) <- toList t0]
    fitCheck    = totalFilled <= uncurry (*) dim
    bounds0     = (pieceSize, pieceSize)

    go :: Task -> Pos -> Occ -> [()]
    go !ts bounds@(!ib, !jb) !board
      | null rem  = pure ()
      | otherwise = do
        Just locals   <- pure $ traverse (_2 %%~ restrictLocalValid) rem
        let pieceType = minimumBy (comparing $ length . snd . snd) locals
        let (taskId, (_, pickFrom)) = pieceType
        (occ, (i, j)) <- pickFrom
        let board'  = board .|. occ
        let bounds' = (max ib (i + 3), max jb (j + 3))
        let !ts' = ts & ix taskId . _1 -~ 1
        go ts' bounds' board'
      where
        rem = remaining ts

        inBounds :: Pos -> Bool
        inBounds (i, j) = i <= fst bounds && j <= snd bounds
    
        restrictLocalValid :: Constr -> Maybe Constr
        restrictLocalValid (cnt, pi) = case pls of
          [] -> Nothing
          _  -> Just (cnt, pls)
          where
            pls =
              [(occ, pos) | (occ, pos) <- pi, inBounds pos, disjoint occ board]
    
    remaining :: Task -> [(Int, Constr)]
    remaining v = [(i, c) | (i, c) <- li, fst c > 0]
      where
        li = [0..] `zip` toList v

disjoint :: Occ -> Occ -> Bool
disjoint a b = a .&. b == 0

part1 :: String -> String
part1 s = filter id results & length & show
  where
    (pieces, boards) = fromJust (M.parseMaybe parser1 s)
    pisIndexed       = second mkPie <$> pieces
    results          = solveBoard pisIndexed <$> boards

part2 :: String -> String
part2 = const "no part 2 :)"

main :: IO ()
main = AOC.wrapInteract 12 (AOC.combineParts part1 part2)
