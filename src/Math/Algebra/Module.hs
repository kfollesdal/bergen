-- TODO
-- @ Documentation
-- @ Add tests to ModuleTest.hs
-- @ Use ⨂ as symbol for Tensor product
-- @ Left, Rigth, bimodule?
-- @ Make own lienar data type ~> for linerar maps? Memotier?
-- @ Monad and bilinear
-- @ If we make [(b, k)] an instance of FreeModule, then we can define decompose
--   and compose by linear.

{-# LANGUAGE
  NoImplicitPrelude,
  TypeFamilyDependencies,
  FlexibleContexts,
  FlexibleInstances,
  ViewPatterns,
  MultiParamTypeClasses,
  UndecidableInstances
#-}
--

{-|
Module      : Math.Module
Description : Defenition of mathemaical Module for Haskell.
Maintainer  : Kristoffer K. Føllesdal <kfollesdal@gmail.com>
-}

module Math.Algebra.Module (
  -- * Module
  Module (..),

  -- * Free Module
  FreeModule (..),
  bilinear,

  -- * Tensor
  Tensor,
  HasTensorProduct (..)
) where

import Math.Algebra.Ring (CommutativeRing)
import Math.Algebra.Group (AbelianGroup)
import Math.Algebra.Monoid ((*), sum)
import GHC.Base (Functor (..), (.))

class (AbelianGroup m, CommutativeRing (Scalar m)) => Module m where
  type Scalar m :: *
  (*>) :: Scalar m -> m -> m

-- FreeModule and hasBasis is the same
class FreeModule f where
  i :: b -> f b
  linear :: (Module (f b), Module m, Scalar (f b) ~ Scalar m) => (b -> m) -> f b -> m
  decompose :: (Module (f b)) => f b -> [(b, Scalar (f b))]
  compose :: (Module (f b)) => [(b, Scalar (f b))] -> f b

-- instance (FreeModule f, Module (f a), Module (f b), Scalar (f a) ~ Scalar (f b)) => Functor f where
--   fmap f = linear (i.f)

bilinear :: (FreeModule f, Module l, Module (f a), Module (f b), Scalar (f a) ~ Scalar l, Scalar (f b) ~ Scalar l) => (a -> b -> l) -> (f a -> f b -> l)
bilinear g xs ys = linear (\x -> linear (g x) ys ) xs

type family Tensor m n = x | x -> m n
type instance Tensor (f a) (f b) = f (a,b)

class (Module m, Module n, Scalar m ~ Scalar n, Module (Tensor m n), Scalar (Tensor m n) ~ Scalar m) => HasTensorProduct m n where
  te :: m -> n -> Tensor m n
  tf :: (HasTensorProduct m' n', Module m', Module n', Scalar m' ~ Scalar n', Scalar m' ~ Scalar m, Module (Tensor m' n'), Scalar (Tensor m' n') ~ Scalar m) => (m -> m') -> (n -> n') -> Tensor m n -> Tensor m' n'

te_ :: (FreeModule f) => a -> b -> Tensor (f a) (f b)
te_ x y = i (x,y)

instance (FreeModule f, Module (f a), Module (f b), Module (Tensor (f a) (f b)), Scalar (f a) ~ Scalar (f b), Scalar (f a) ~ Scalar (f (a,b))) => HasTensorProduct (f a) (f b) where --Scalar (Tensor m' n') ~ Scalar (f a)
  te = bilinear (\x y -> te_ x y)
  tf f g = linear (\(x,y) -> te (f (i x)) (g (i y)))

tfc :: (FreeModule f, Module (f a), Module (f b), Module (f c), Module (f a'), Module (f b'), Module (f c'),
        Module (Tensor (f a) (f b)), Module (Tensor (f a') (f b')),
        Module (Tensor (f a) (f a')), Module (Tensor (f b) (f b')),
        Module (Tensor (Tensor (f a) (f a')) (Tensor (f b) (f b'))), Module (Tensor (f c) (f c')),
        Scalar (f b) ~ Scalar (f a), Scalar (f c) ~ Scalar (f a), Scalar (f a') ~ Scalar (f a), Scalar (f b') ~ Scalar (f a), Scalar (f c') ~ Scalar (f a),
        Scalar (Tensor (f a) (f b)) ~ Scalar (f a), Scalar (Tensor (f a') (f b')) ~ Scalar (f a),
        Scalar (Tensor (f a) (f a')) ~ Scalar (f a), Scalar (Tensor (f b) (f b')) ~ Scalar (f a),
        Scalar (Tensor (Tensor (f a) (f a')) (Tensor (f b) (f b'))) ~ Scalar (f a), Scalar (Tensor (f c) (f c')) ~ Scalar (f a))
    => (Tensor (f a) (f b) -> f c) -> (Tensor (f a') (f b') -> f c')
    -> Tensor (Tensor (f a) (f a')) (Tensor (f b) (f b')) -> Tensor (f c) (f c')
tfc h g = linear (\((x,x'),(y,y')) -> te (h (te_ x y)) (g (te_ x' y')))
