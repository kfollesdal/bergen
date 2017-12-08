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
import Math.Algebra.Ring

data MK c = I | MK c (MK c) (MK c)


class (Collection (Forrest t), t ~ Element (Forrest t), MultiplicativeMonoid (Forrest t)) => PlanarTree t where
  type Node t
  type Forrest t
  root :: t -> Node t
  children :: t -> Forrest t
  node :: Node t -> Forrest t -> t
  -- value(node(e, f)) = e
  -- children(node(e, f)) = f

class (PlanarTree t, CommutativMonoid (Forrest t)) => NonPlanarTree t where

bminus :: (PlanarTree t) => t -> Forrest t
bminus = children

bplus :: (PlanarTree t) => Node t -> Forrest t -> t
bplus = node

data TreeD p c = TreeD c [TreeD p c] deriving (Show, Eq)

instance MultiplicativeMonoid [a] where
  u = []
  xs * ys = xs ++ ys

instance (Eq c) => PlanarTree (TreeD p c) where
  type Node (TreeD p c) = c
  type Forrest (TreeD p c) = [TreeD p c]
  root (TreeD x _) = x
  children (TreeD _ xs) = xs
  node x xs = TreeD x xs

data NonPlanar

--instance Ord (TreeD p c) where




-- class (Collection f, Tree (Element f)) => Forrest f where
--   isTree :: f -> Bool
--   bplus :: Node (Element f) -> f -> Element f
--   bminus :: Element f -> f
--
-- class (Forrest f, FreeMonoid f) => OrderedForrest f where
--   mk_ :: Node(Element f) -> f -> f -> f
--   mk_ c x y = x * (singelton (bplus c y))
--   mkSplit :: f -> (Node(Element f),f,f)
--   mkSplit w = (mkRoot w, mkLeft w, mkRigth w)
--   mkRoot :: f -> Node(Element f)
--   mkRoot (mkSplit -> (c,_,_)) = c
--   mkLeft :: f -> f
--   mkLeft (mkSplit -> (_,l,_)) = l
--   mkRigth :: f -> f
--   mkRigth (mkSplit -> (_,_,r)) = r
--
--
-- mkT :: (OrderedForrest f, CommutativeRing k, Eq k, Ord f) => Node (Element f) -> Tensor (FreeModule k f) (FreeModule k f) -> FreeModule k f
-- mkT c = linear (\(x,y) -> basis (mk_ c x y))
--
--
-- mk :: (OrderedForrest f, CommutativeRing k, Eq k, Ord f) => Node(Element f) -> FreeModule k f -> FreeModule k f -> FreeModule k f
-- mk c = bilinear (\x y -> basis (mk_ c x y))
--
-- data MK c = I | MK {root:: c, left:: MK c, rigth :: MK c} deriving (Eq, Ord, Show)
--
