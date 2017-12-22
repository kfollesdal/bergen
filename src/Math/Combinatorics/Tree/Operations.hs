{-# LANGUAGE
  NoImplicitPrelude,
  ViewPatterns,
  TypeSynonymInstances,
  FlexibleInstances,
  InstanceSigs,
  UndecidableInstances
#-}

module Math.Combinatorics.Tree.Operations where

import Math.Combinatorics.Tree
import Math.Algebra.Module.FreeModule
import Math.Algebra.Module
import Math.Algebra.Monoid
import Math.Algebra.Group
import GHC.Base (Bool (..), Ord, Eq, Functor (..), ($), (.), (==))
import Math.Algebra.Ring
import Math.TEMP.HasEmpty
import Data.List (sortBy, map)
import Data.Ord

type ModuleForest k t = FModule k (Forest t)
type ModuleOrdForest k n = FModule k (OrderedForest n)

mk :: (Eq n, Eq k, Ord n, CommutativeRing k) => n -> ModuleOrdForest k n -> ModuleOrdForest k n -> ModuleOrdForest k n
mk c = bilinear (\x y -> i (mk_ c x y))

class (Ord t) => GraftProduct t where
  graft_, (⊵) :: (CommutativeRing k, Eq k) => Forest t -> Forest t -> ModuleForest k t
  (⊵) = graft_
  -- graft :: Tensor (FModule k f) (FModule k f) -> FModule k f
  -- graft = tensorBilinear graft_
  graft', (▷) :: (CommutativeRing k, Eq k) => ModuleForest k t -> ModuleForest k t -> ModuleForest k t
  graft' = bilinear graft_
  (▷) = graft'

instance (Ord n) => GraftProduct (PlanarTree n) where
  --graft_:: (CommutativeRing k) => Forest n -> Forest n -> ModuleForest k n
  graft_ (isEmpty -> True) w = i w
  graft_ v (isEmpty -> True) = zero
  graft_ t@(isTree -> True) (mkSplit -> (c, l, r)) =
     mk c (t ⊵ l) (i r) + mk c (i l) (i (t*r) + (t ⊵ r))
  graft_ (headTail -> (t,f)) w = i t ▷ (f ⊵ w) - (t ⊵ f) ▷ i w

-- return trees after how they are buiult withe left grafing and not on normalform. Use nf to get on normalform.
instance (Ord n) => GraftProduct (NonPlanarTree n) where
  graft_ x y = compose (decompose $ fmap nonplanar $ graft_ (planar x) (planar y)) -- add nf in front

instance (CommutativeRing k, Ord n, Eq k) =>  NormalForm (FModule k (Forest (NonPlanarTree n))) where
  nf (decompose -> xs)  = compose (map (\(b,c) -> (nf b,c)) xs)
    -- temp $ sortBy (\(x,_) (y,_) -> compare x y) (map (\(x,c) -> (nf x, c)) xs) where
    -- temp xs = sum [c *> i x | (x,c) <- xs]

-- instance (Ord b, NormalForm b, CommutativeRing k, Eq k) => NormalForm (FModule k b) where
--   nf (FM xs) = FM $ sortBy (\ (x,_) (y,_) -> compare x y) (map (\(x,c) -> (nf x, c)) xs)
  -- where
  --   nf' [(b,c)] = if c == zero then zero else FM [(b,c)]
  --   nf' ((b1,c1):(b2,c2):xs)
  --     | b1 == b2 = if c1 + c2 == zero then nf' xs else (b1,c1 + c2):(nf' xs)
  --     | b1 < b2 = if c1 == zero then nf' ((b2,c2):xs) else (b1,c1):(nf' ((b2,c2):xs))
  --   nf' [] = zero

headTail :: [a] -> ([a],[a])
headTail (x:xs) = ([x] ,xs)

-- instance (Ord n) => GraftProduct (NonPlanar (Forest n)) where
--   graft_ (NonPlanar x) (NonPlanar y) = fmap (\x -> NonPlanar x) (graft_ x y)




-- graft', (▷) :: (CommutativeRing k, Ord n, Eq n, Eq k) => ModuleForest k n -> ModuleForest k n -> ModuleForest k n
-- graft' = bilinear graft_
-- (▷) = graft'












-- instance (Forrest f) ⇒ GraftingProduct f where
--   graft_ (isEmpty -> True) w = lift w
--   graft_ w (isEmpty -> True) = zero
--   graft_ t@(isTree -> True) (mkSplit -> (c, l, r)) =
--     mk c (t ⊵ l) (lift r) + mk c (lift l) (lift (u⋅r) + t ⊵ r)
--   graft_ (headTail -> (t,f)) w = (lift t) ▷ (f ⊵ w) - (t ⊵ f) ▷ (lift w)
--
-- gl_ :: t -> t -> m t
-- gl x (isEmpty -> True) = i x
-- gl (isEmpty -> True) y = i y
-- gl x w@(mkRoot -> c) = fmap bMinus (graft_ x (bPlus c y))
--
-- pruning_ :: t -> Tensor (m t) (m t)
-- pruning_ I = te_ I I
-- pruning_ (mkSplit -> (c,l,r)) = tfc shuffle (mk c) (te (pruning_ l) (dualGl_ r))
