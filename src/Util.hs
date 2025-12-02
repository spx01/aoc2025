module Util where

import Data.Maybe

newtype TInt a = TInt { getTInt :: Int }
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral)

head' :: [a] -> a
head' = fromJust . listToMaybe
