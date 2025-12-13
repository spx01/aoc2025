module Util
  ( TInt(..), head', ifM, breakDrop, uncons', FoldL, FoldR, filterWithIndex
  , seqTrans, Endo', rotL, shead', fromBitIxs
  )
where

import qualified Data.Sequence as S
import Data.Sequence (Seq(..))
import Data.Maybe
import Control.Arrow
import Data.List
import Data.Foldable
import Data.Bits

newtype TInt a = TInt { getTInt :: Int }
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral)

head' :: [a] -> a
head' = fromJust . listToMaybe

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM b t e = b >>= \case { True -> t; _ -> e }

breakDrop :: (a -> Bool) -> [a] -> ([a], [a])
breakDrop p = break p >>> second (drop 1)

uncons' :: [a] -> (a, [a])
uncons' = fromJust . uncons

type FoldL acc a = acc -> a -> acc
type FoldR a acc = a -> acc -> acc
type Endo' a = a -> a

filterWithIndex :: (Int -> a -> Bool) -> Seq a -> Seq a
filterWithIndex f = S.mapWithIndex (,) >>> S.filter (uncurry f) >>> fmap snd

seqTrans :: Seq (Seq a) -> Seq (Seq a)
seqTrans = toList >>> fmap toList >>> transpose >>> fmap S.fromList >>> S.fromList

shead' :: Seq a -> a
shead' (x :<| _) = x
shead' _ = undefined

rotL :: Int -> Seq a -> Seq a
rotL _ Empty = Empty
rotL n s
  | n < 0, xs :|> x <- s = rotL (n + 1) (x :<| xs)
  | n > 0, x :<| xs <- s = rotL (n - 1) (xs :|> x)
  | otherwise = s

fromBitIxs :: (Bits a, Foldable f) => f Int -> a
fromBitIxs =
  getIor . foldMap (\i -> if i >= 0 then Ior (bit i) else Ior zeroBits)
