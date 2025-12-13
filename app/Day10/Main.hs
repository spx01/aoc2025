{-# LANGUAGE NoOverloadedStrings #-}

module Main (main) where

import AOC
import Util

import qualified Data.SBV.Set as SE
import qualified Data.Set as SE'
import qualified Data.Sequence as S
import Data.Sequence (Seq(..))
import Data.List.Split
import Control.Arrow
import Data.Bits
import Data.Function
import Data.Foldable
import Data.SBV

type Puzzle = (Int, [Int])
type BMat   = Seq (Seq Bool)
type Vec    = Seq Int
type Sol1   = [Int]

fromBits :: [Bool] -> Int
fromBits bs = sum [ bit i | (b, i) <- zip bs [0 ..], b ]

parseSections :: String -> S.Seq String
parseSections = S.fromList . words . filter (`notElem` "[](){}")

parseLights :: String -> Int
parseLights = fmap (== '#') >>> fromBits

parseButton :: String -> Int
parseButton = splitOn "," >>> fmap (read @Int) >>> toList >>> fromBitIxs

parsePuzzle :: String -> Puzzle
parsePuzzle s = (solved, toList buttons)
  where
    (solved, buttons) =
      case parseSections s of
        (s0 :<| sb) :|> _ -> (parseLights s0, parseButton <$> sb)
        _ -> undefined

toBitsPadded :: Int -> Int -> [Bool]
toBitsPadded = go
  where
    go :: Int -> Int -> [Bool]
    go p 0 = replicate p False
    go p n = odd n : go (p - 1) (n .>>. 1)

toBMat :: Int -> [Int] -> BMat
toBMat pad ns = seqTrans m0
  where
    m0 = fmap (S.fromList . toBitsPadded pad) ns & S.fromList

parseSystem :: String -> (BMat, Vec)
parseSystem s = (toBMat (length counts) (toList buttons), counts)
  where
    (buttons, counts) =
      case parseSections s of
        (_ :<| sb) :|> sc -> (parseButton <$> sb, parseCounts sc)
        _ -> undefined
    parseCounts = splitOn "," >>> fmap (read @Int) >>> S.fromList

part1 :: String -> IO String
part1 s = show . sum <$> traverse go ps
  where
    ps = parsePuzzle <$> lines s
    go p = length <$> solvePuzzle p

part2 :: String -> IO String
part2 s = show . sum <$> traverse go sys
  where
    sys = parseSystem <$> lines s
    go s = sum <$> uncurry solveSystem s

main :: IO ()
main = AOC.wrapInteract 10 (AOC.combineParts' part1 part2)

varNames :: Int -> [String]
varNames n = fmap (\i -> "x" ++ show i) [0 .. n - 1]

mkSystem :: BMat -> Vec -> ([String], ConstraintSet)
mkSystem ma b = (vars,) $ do
  x <- S.fromList <$> mapM sInteger vars
  forM_ x $ \xi -> constrain $ xi .>= 0
  forM_ (ma `S.zip` b) $ \(c, t) -> do
    let vars = [xi | (xi, b) <- toList (x `S.zip` c), b]
    constrain $ sum vars .== literal (fromIntegral t)
  minimize "goal" $ sum x
  where
    mt    = seqTrans ma
    nVars = length mt
    vars  = varNames nVars

solveSystem :: BMat -> Vec -> IO Vec
solveSystem ma b = do
  let (vars, sys) = mkSystem ma b
  res <- optimize Lexicographic sys
  case res of
    LexicographicResult r@Satisfiable{} -> do
      let vals = traverse (flip (getModelValue @SMTResult @Integer) r) vars
      case vals of
        Just vs -> pure . S.fromList . fmap fromIntegral $ vs
        _ -> undefined
    _ -> undefined

charac :: (SymVal a, Mergeable b, Ord a)
  => (a -> b) -> (a -> b) -> [a] -> SSet a -> [b]
charac f t u s = [ite (literal x `SE.member` s) (t x) (f x) | x <- u]

mkPuzzle :: Puzzle -> ConstraintSet
mkPuzzle (fs0, bs0) = do
  (s :: SSet Int32) <- sSet "buttonSet"
  constrain $ s `SE.isSubsetOf` fullButtonSet
  constrain $ reduced s .== 0
  minimize "goal" $ setSize s
  where
    fullButtonSet :: SSet Int32
    fullButtonSet = SE.fromList bs

    setSize :: SSet Int32 -> SInt32
    setSize s = sum (charac (const 0) (const 1) bs s)

    reduced :: SSet Int32 -> SInt32
    reduced s = foldl' xor (literal fs) (charac (const 0) literal bs s)

    bs = fmap fromIntegral bs0
    fs = fromIntegral fs0

solvePuzzle :: Puzzle -> IO Sol1
solvePuzzle p = do
  r <- optimize Lexicographic (mkPuzzle p)
  case r of
    LexicographicResult r@Satisfiable{} -> do
      case getModelValue @SMTResult @(RCSet Int32) "buttonSet" r of
        Just (RegularSet s) -> pure . fmap fromIntegral $ SE'.toList s
        _ -> undefined
    _ -> undefined

{-
>>> (parsePuzzle "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")
(6,[8,10,4,12,5,3])

>>> optimize Lexicographic (mkPuzzle (parsePuzzle "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"))
Optimal model:
  buttonSet           =      {3,5} :: {Int32}
  toMetricSpace(goal) = 2147483650 :: Word32
  goal                =          2 :: Int32
-}
