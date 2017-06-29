module TestLib where

import Library
import Stringify


data ComparableReal = CReal Double

instance Eq ComparableReal where
  (CReal x) == (CReal y) = closeTo x y

instance Show ComparableReal where
  show (CReal x) = stringify x


toCReal :: Functor a => a (a Double) -> a (a ComparableReal)
toCReal = fmap (fmap CReal)


toCReal2 :: (Functor a, Functor b) => b (a Double) -> b (a ComparableReal)
toCReal2 = fmap (fmap CReal)


toCReal3 :: (Functor a, Functor b) => b (a (a Double)) -> b (a (a ComparableReal))
toCReal3 = fmap (fmap (fmap CReal))
