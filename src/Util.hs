module Util
  ( TInt(..), head', ifM, breakDrop, uncons', FoldL, FoldR, filterWithIndex
  , seqTrans
  )
where

import qualified Data.Sequence as S
import Data.Maybe
import Control.Arrow
import Data.List
import Data.Foldable

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

filterWithIndex :: (Int -> a -> Bool) -> S.Seq a -> S.Seq a
filterWithIndex f = S.mapWithIndex (,) >>> S.filter (uncurry f) >>> fmap snd

seqTrans :: S.Seq (S.Seq a) -> S.Seq (S.Seq a)
seqTrans = toList >>> fmap toList >>> transpose >>> fmap S.fromList >>> S.fromList
