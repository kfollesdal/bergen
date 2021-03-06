-- TODO
-- @ List is not a good data type for forest. Not effective to pick out last element
--   e.g mkRoot and mkRigth.
-- @ Check ordering for Planar and NonPlanarTree
-- @ Use Planar or Ordered for forests?

{-# LANGUAGE
  NoImplicitPrelude,
  ViewPatterns,
  TypeFamilies,
  MultiParamTypeClasses,
  FunctionalDependencies,
  TypeSynonymInstances,
  FlexibleInstances,
  UndecidableInstances
#-}

{-|
Module      : Math.Combinatorics.Tree
Description : Trees, planar and nonplanar.
Maintainer  : Kristoffer K. Føllesdal <kfollesdal@gmail.com>
-}

-- FreeModule Monad
-- change name basis to lift

module Math.Combinatorics.Tree where

import GHC.Base ((&&), (.), id, Eq (..), Ord (..), Ordering(..), Bool (..), otherwise, error, ($),(++), undefined, Int)
import Data.List (init, last, length, filter, zipWith, head, map, sort, maximum, concatMap)
import Text.Show (Show(..))
import Math.Algebra.Module
import Math.Algebra.Module.FreeModule
import Data.Monoid
import Math.TEMP.HasEmpty
import Math.TEMP.Collection
import Math.Algebra.Monoid
-- import Math.TEMP.FreeMonoid
import Math.Algebra.Ring

class Tree t where
  type Node t :: *
  root :: t -> Node t
  branches :: t -> Forest t
  join :: Node t -> Forest t -> t

cut :: (Tree t, r ~ Node t) => t -> (r, Forest t)
cut x = (root x, branches x)

instance (Tree t) => Size t where
  size x = 1 + sum (map size (branches x))

instance (Tree t, Eq t) => Height t where
  height (isEmpty.branches -> True) = 1
  height (cut -> (x,xs)) = 1 + maximum (map height xs)

instance (u ~ Node t, Tree t) => ToList t u where
  toList (cut -> (x,xs)) = x : concatMap toList xs

-- PLANAR TREES
data PlanarTree n = R n (Forest (PlanarTree n)) deriving (Show, Eq)

instance Tree (PlanarTree n) where
  type Node (PlanarTree n) = n
  root (R x xs) = x
  branches (R x xs) = xs
  join = R

-- Planar trees are order by left grafitng.
instance (Eq n, Ord n) => Ord (PlanarTree n) where
  compare x y = case compare (size x) (size y) of
    LT -> LT
    GT -> GT
    EQ -> case compare (length (branches x)) (length (branches y)) of
      LT -> GT
      GT -> LT
      EQ -> case compare (height x) (height y) of
        LT -> LT
        GT -> GT
        EQ -> let cs = filter (/= EQ) (zipWith compare (branches x) (branches y))
              in if cs == []
                then compare (toList x) (toList y)
                else case head cs of
                  LT -> GT
                  GT -> LT

-- NONPLANAR TREES
newtype NonPlanarTree n = NP {getPlanar :: PlanarTree n}

instance Tree (NonPlanarTree n) where
  type Node (NonPlanarTree n) = n
  root = root . getPlanar
  branches = map NP . branches . getPlanar
  join x xs = NP (R x (map planar xs))

instance (Eq n, Ord n) => Eq (NonPlanarTree n) where
  x == y = planar (nf x) == planar (nf y)

instance (Ord n) => Ord (NonPlanarTree n) where
  compare x y = case compare (size x) (size y) of
    LT -> LT
    GT -> GT
    EQ -> case compare (length (branches x)) (length (branches y)) of
      LT -> GT
      GT -> LT
      EQ -> compare (branches x) (branches y)

instance {-# OVERLAPPING #-} (Eq n, Ord n) => Eq (Forest (NonPlanarTree n)) where
  xs == ys = sort (map (planar.nf) xs) == sort (map (planar.nf) ys)

instance {-# OVERLAPPING #-} (Ord n) => Ord (Forest (NonPlanarTree n)) where
  compare xs ys = compare (sort (map (planar.nf) xs)) (sort (map (planar.nf) ys))

-- FOREST
type Forest t = [t]
type PlanarForest n = Forest (PlanarTree n)
type NonPlanarForest n = Forest (NonPlanarTree n)

isTree :: (Tree t) => Forest t -> Bool
isTree x = length x == 1

-- -- Munte-Kaas product for ordered forests
-- -- Make [a] free monoid and use i insted of singelton, make class free.
mk_ :: (Eq n) => n -> PlanarForest n -> PlanarForest n  -> PlanarForest n
mk_ x fs gs = fs * singelton (join x gs)

-- Does not work for empty forrest
mkRoot :: PlanarForest n -> n
mkRoot = root . last

mkLeft :: PlanarForest n -> PlanarForest n
mkLeft [] = []
mkLeft xs = init xs

mkRigth :: (Eq n) => PlanarForest n -> PlanarForest n
mkRigth xs = branches $ last xs -- singelton -> i, after [a] is free. can then remove Eq n

mkSplit :: (Eq n) => PlanarForest n -> (n, PlanarForest n, PlanarForest n) -- Remove Eq n when remov Eq n from mkRigth
mkSplit xs = (mkRoot xs, mkLeft xs, mkRigth xs)














class Planar a b | a -> b where
  planar :: a -> b

instance Planar (NonPlanarTree n) (PlanarTree n) where
  planar (NP x) = x

-- Can not have type family in class instance
instance Planar (NonPlanarForest n) (PlanarForest n) where
  planar = map planar


--
-- headTail :: Forest n -> (Forest n , Forest n)
-- headTail (x:xs) = ([x],xs)
--
-- data Planar
-- data NonPlanar
--


class NonPlanarFun a b | a -> b where
  nonplanar :: a -> b

instance NonPlanarFun (PlanarTree n) (NonPlanarTree n) where
  nonplanar = NP

instance NonPlanarFun (PlanarForest n) (NonPlanarForest n) where
  nonplanar = map nonplanar




class NormalForm a where
  nf :: a -> a

instance (Ord n) => NormalForm (PlanarTree n) where
  nf (R n fs) = R n (sort (map nf fs))

instance (Ord n) => NormalForm (NonPlanarTree n) where
  nf (NP t) = NP (nf t)

instance (NormalForm t, Ord t) => NormalForm [t] where
  nf xs = (sort (map nf xs))




--
--
-- -- mkT :: (OrderedForrest f, CommutativeRing k, Eq k, Ord f) => Node (Element f) -> Tensor (FreeModule k f) (FreeModule k f) -> FreeModule k f
-- -- mkT c = linear (\(x,y) -> basis (mk_ c x y))
-- --
-- --
-- -- mk :: (OrderedForrest f, CommutativeRing k, Eq k, Ord f) => Node(Element f) -> FreeModule k f -> FreeModule k f -> FreeModule k f
-- -- mk c = bilinear (\x y -> basis (mk_ c x y))







---------------------
-- class Split a b | a -> b where
--   split :: a -> b

class Size a where
  size :: a -> Int

class Height a where
  height :: a -> Int

class ToList a b where
  toList :: a -> [b]


-- type family Forest t = f | f -> t where
--   Forest (PlanarTree n) = [PlanarTree n]
--   Forest (NonPlanarTree n) = [NonPlanarTree n]
