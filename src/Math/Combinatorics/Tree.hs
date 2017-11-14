{-# LANGUAGE
  NoImplicitPrelude,
  UnicodeSyntax,
  TypeFamilies,
  ViewPatterns,
  FlexibleInstances,
  MultiParamTypeClasses,
  FlexibleContexts,
  TypeOperators
#-}

-- FreeModule Monad
-- change name basis to lift

module Math.Combinatorics.Tree where

import GHC.Base (id, Eq (..), Ord (..), Bool (..), otherwise, error, ($),(++), undefined, Int)
import Text.Show (Show(..))
import Math.Algebra.Module
import Math.Algebra.Module.FreeModule
import Data.Monoid
import Math.TEMP.HasEmpty
import Math.TEMP.Collection
import Math.Algebra.Monoid
import Math.TEMP.FreeMonoid

class Tree t where
  type Node t

class (Collection f, Tree (Element f)) => Forrest f where
  isTree :: f -> Bool
  bplus :: Node (Element f) -> f -> Element f
  bminus :: Element f -> f

class (Forrest f, FreeMonoid f) => OrderedForrest f where
  mk_ :: Node(Element f) -> f -> f -> f
  mk_ c x y = x * (singelton (bplus c y))
  mkSplit :: f -> (Node(Element f),f,f)
  mkSplit w = (mkRoot w, mkLeft w, mkRigth w)
  mkRoot :: f -> Node(Element f)
  mkRoot (mkSplit -> (c,_,_)) = c
  mkLeft :: f -> f
  mkLeft (mkSplit -> (_,l,_)) = l
  mkRigth :: f -> f
  mkRigth (mkSplit -> (_,_,r)) = r

--class (Collection f, Tree(Element f), FreeMonoid f) => Forrest f where


  -- mkT :: Node (Element f) -> FreeModule k f ⨂ FreeModule k f -> FreeModule k f
  -- mkT c = linear (\(x,y) -> basis (mk_ c x y))


-- mk :: (Forrest f) => Node(Element f) -> FreeModule k f -> FreeModule k f -> FreeModule k f
-- mk c = bilinear (mk_ c)

data MK c = I | MK {root:: c, left:: MK c, rigth :: MK c} deriving (Eq, Ord, Show)

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
--   graft_ (headTail -> (t,f)) w = (lift t) ▷ (f ⊵ w) - (t ⊵ f) ▷ (lift w)
