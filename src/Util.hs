module Util where

import Data.Maybe

newtype TInt a = TInt { getTInt :: Int }
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral)

head' :: [a] -> a
head' = fromJust . listToMaybe

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM b t e = b >>= \case { True -> t; _ -> e }
