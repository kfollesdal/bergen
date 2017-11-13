{-# LANGUAGE
  NoImplicitPrelude,
  UnicodeSyntax,
  TypeFamilies,
  ViewPatterns,
  FlexibleInstances,
  MultiParamTypeClasses,
  FlexibleContexts
#-}

-- FreeModule Monad
-- change name basis to lift

module Math.Combinatorics.Tree where

import GHC.Base (id, Eq (..), Ord (..), Bool (..), otherwise, error, ($),(++), undefined, Int)
import Text.Show (Show(..))
import Math.Algebra.Module
import Math.Algebra.Module.FreeModule
import Data.Monoid

class Enumerable e where
  count :: e -> Int

class (Eq e) => HasEmpty e where
  empty :: e
  isEmpty :: e -> Bool
  isEmpty x
    | x == empty = True
    | otherwise = False

class (HasEmpty c) => Collection c where
  type Element c
  insert :: Element c -> c -> c
  singelton :: Element c -> c
  singelton x = insert x empty

instance (Eq a) => HasEmpty [a] where
  empty = []

instance (Eq a) => Collection [a] where
  type Element [a] = a
  insert x ce = x:ce

class (Enumerable c, Collection c) => EnumerableCollection c where
  toList :: c -> [Element c]

class Tree t where
  type Node t

class (Collection f, Tree(Element f), MultiplicativeMonoid f) => Forrest f where
  isTree :: f -> Bool
  bplus :: Node (Element f) -> f -> Element f
  bminus :: Element f -> f
  mk_ :: Node(Element f) -> f -> f -> f
  mk_ c x y = x ⋅ bplus c y
  mkSplit :: f -> (Node(Element f),f,f)
  mkSplit w = (mkRoot w, mkLeft w, mkRigth w)
  mkRoot :: f -> Node(Element f)
  mkRoot (mkSplit -> (c,_,_)) = c
  mkLeft :: f -> f
  mkLeft (mkSplit -> (_,l,_)) = l
  mkRigth :: f -> f
  mkRigth (mkSplit -> (_,_,r)) = r
  mkT :: Node (Element f) -> FreeModule k f ⨂ FreeModule k f -> FreeModule k f
  mkT c = linear (\(x,y) -> basis (mk_ c x y))
  mk :: Node(Element f) -> FreeModule k f -> FreeModule k f -> FreeModule k f
  mk c = bilinear mk_ c
  --unconcat :: f -> (Tree f, f)


data MK c = I | MK {root:: c, left:: MK c, rigth :: MK c} deriving (Eq, Ord, Show)

instance (Eq c) => HasEmpty (MK c) where
  empty = I

-- instance (Eq c) => Collection (MK c) where
--   type Element (MK c) = MK c


-- instance (Forrest f) => Monoid (f c) where
--   mempty = empty
--   mappend = concat

-- class GraftingProduct f where
--   graft_, (⊵) :: f → f → FreeModule k f
--   graft :: (FreeModule k f) ⨂ (FreeModule k f) → FreeModule k f
--   graft = tensorBilinear graft_
--   graft', (▷) :: FreeModule k f → FreeModule k f → FreeModule k f
--   graft' = bilinear graft_
--
-- instance (Forrest f) ⇒ GraftingProduct f where
--   graft_ (isEmpty -> True) w = lift w
--   graft_ w (isEmpty -> True) = zero
--   graft_ t@(isTree -> True) (mkSplit -> (c, l, r)) =
--     mk c (t ⊵ l) (lift r) + mk c (lift l) (lift (u⋅r) + t ⊵ r)
--   graft_ (unconcat -> (t,f)) w = (lift t) ▷ (f ⊵ w) - (t ⊵ f) ▷ (lift w)
