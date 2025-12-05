module Util (TInt(..), head', ifM, breakDrop) where

import Data.Maybe
import Control.Arrow
newtype TInt a = TInt { getTInt :: Int }
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral)

head' :: [a] -> a
head' = fromJust . listToMaybe

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM b t e = b >>= \case { True -> t; _ -> e }

breakDrop :: (a -> Bool) -> [a] -> ([a], [a])
breakDrop p = break p >>> second (drop 1)
