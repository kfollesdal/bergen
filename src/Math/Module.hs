-- TODO
-- @ Documentation
-- @ Add tests to ModuleTest.hs
-- @ Use ⨂ as symbol for Tensor product
-- @ Left, Rigth, bimodule?
-- @ Monad and bilinear
-- @ Add a overloaded lift function of function on basis (Basis -> Module -> Module, Module -> Bais -> Module ..)
-- @ In HasTensorProduct way do the follwoing code not work?
--   te_ :: (Basis (Tensor m n) ~ (Basis m, Basis n)) => Basis m -> Basis n -> Tensor m n
--   te_ x y = basis (x,y)
--   te :: (Tensor (Basis m) (Basis n) ~ Tensor m n, Basis (Tensor m n) ~ (Basis m, Basis n)) => m -> n -> Tensor m  n
--   te (decompose -> xs) (decompose -> ys) = msum [(a*b) *> te_ x y| (x,a) <-xs, (y,b) <- ys]

{-# LANGUAGE
  NoImplicitPrelude,
  TypeFamilies,
  FlexibleContexts,
  ViewPatterns,
  MultiParamTypeClasses
#-}

{-|
Module      : Math.Module
Description : Defenition of mathemaical Module for Haskell.
Maintainer  : Kristoffer K. Føllesdal <kfollesdal@gmail.com>
-}

module Math.Module (
  -- * Module
  Module (..),

  -- * Basis
  HasBasis (..),

  -- * Tensor
  HasTensorProduct (..)
) where

import Math.Ring (CommutativeRing)
import Math.Group (AbelianGroup)
import Math.Monoid ((*), msum)

class (AbelianGroup m, CommutativeRing (Scalar m)) => Module m where
  type Scalar m :: *
  (*>) :: Scalar m -> m -> m

class (Module m) => HasBasis m where
  type Basis m :: *
  basis :: Basis m -> m
  decompose :: m -> [(Basis m, Scalar m)]
  decompose' :: m -> Basis m -> Scalar m
  linearCombi :: [(Basis m, Scalar m)] -> m
  linearCombi  xs = msum [ c *> (basis b) | (b,c) <- xs]

linear :: (HasBasis n, Module m, Scalar n ~ Scalar m) => (Basis n -> m) -> (n -> m)
linear f (decompose -> xs) = msum [ c *> f b | (b,c) <-xs]

bilinear :: (Module m, Module n, Module l,
            Scalar m ~ Scalar n, Scalar n ~ Scalar l,
            HasBasis n, HasBasis m)
            => (Basis m -> Basis n -> l) -> (m -> n -> l)
bilinear f
  (decompose -> xs)
  (decompose -> ys)
  = msum [ (c*d) *> f x y | (x,c) <- xs, (y,d) <- ys]

class (Module m, Module n,
      Scalar m ~ Scalar n, Scalar m ~ Scalar (Tensor m n),
      Basis (Tensor m n) ~ (Basis m, Basis n),
      HasBasis m, HasBasis n, HasBasis (Tensor m n))
      => HasTensorProduct m n where
  type Tensor m n :: *
  te :: m -> n -> Tensor m n
  te (decompose -> xs) (decompose -> ys) = msum [(a*b) *> basis (x,y) | (x,a) <-xs, (y,b) <- ys]
  tf :: (Scalar (Tensor m n) ~ Scalar (Tensor m' n'), HasTensorProduct m' n')
      => (m -> m') -> (n -> n') -> Tensor m n -> Tensor m' n'
  tf f g (decompose -> xs) = msum [k *> te (f (basis x)) (g (basis y)) | ((x,y),k) <- xs]

-- -- HasBasis => FreeModule
-- class (Functor f, Monad f) => FreeModule f where
--   unique :: (Module m) => (a -> f a) -> m -> (a -> m) -> f a -> m
--
-- instance (FreeModule f) => HasBasis (f b) where
--   type Basis (f b) = b
--
-- instance (FreeModule f) => Module (f b) where
