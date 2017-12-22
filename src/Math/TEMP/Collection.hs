{-# LANGUAGE
    NoImplicitPrelude,
    TypeFamilies
#-}

module Math.TEMP.Collection where

import Math.TEMP.HasEmpty
import Math.TEMP.Enumerable
import GHC.Base (Eq (..))

class (HasEmpty c) => Collection c where
  type Element c
  insert :: Element c -> c -> c
  singelton :: Element c -> c
  singelton x = insert x empty

instance (Eq a) => Collection [a] where
  type Element [a] = a
  insert x ce = x:ce

-- class (Enumerable c, Collection c) => EnumerableCollection c where
--   toList :: c -> [Element c]
