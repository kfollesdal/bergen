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
--   te (decompose -> xs) (decompose -> ys) = sum [(a*b) *> te_ x y| (x,a) <-xs, (y,b) <- ys]

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

module Math.Algebra.Module (
  -- * Module
  Module (..),
  linear,
  bilinear,

  -- * Basis
  HasBasis (..),

  -- * Tensor
  HasTensorProduct (..)
) where

import Math.Algebra.Ring (CommutativeRing)
import Math.Algebra.Group (AbelianGroup)
import Math.Algebra.Monoid ((*), sum)

class (AbelianGroup m, CommutativeRing (Scalar m)) => Module m where
  type Scalar m :: *
  (*>) :: Scalar m -> m -> m

class (Module m) => HasBasis m where
  type Basis m :: *
  basis :: Basis m -> m
  decompose :: m -> [(Basis m, Scalar m)]
  decompose' :: m -> Basis m -> Scalar m
  linearCombi :: [(Basis m, Scalar m)] -> m
  linearCombi  xs = sum [ c *> basis b | (b,c) <- xs]

linear :: (HasBasis n, Module m, Scalar n ~ Scalar m) => (Basis n -> m) -> (n -> m)
linear f (decompose -> xs) = sum [ c *> f b | (b,c) <-xs]

bilinear :: (Module m, Module n, Module l,
            Scalar m ~ Scalar n, Scalar n ~ Scalar l,
            HasBasis n, HasBasis m)
            => (Basis m -> Basis n -> l) -> (m -> n -> l)
bilinear f xs ys = linear (\x -> linear (f x) ys ) xs
  --(decompose -> xs)
  -- (decompose -> ys)
  -- = sum [ (c*d) *> f x y | (x,c) <- xs, (y,d) <- ys]*/

class (Module m, Module n,
      Scalar m ~ Scalar n, Scalar m ~ Scalar (Tensor m n),
      Basis (Tensor m n) ~ (Basis m, Basis n),
      HasBasis m, HasBasis n, HasBasis (Tensor m n))
      => HasTensorProduct m n where
  type Tensor m n :: *
  te :: m -> n -> Tensor m n
  te = bilinear (\x y -> basis (x,y))
  -- te (decompose -> xs)
  --    (decompose -> ys)
  --    = sum [(a*b) *> basis (x,y) | (x,a) <-xs, (y,b) <- ys]
  tf :: (Scalar (Tensor m n) ~ Scalar (Tensor m' n'), HasTensorProduct m' n')
      => (m -> m') -> (n -> n') -> Tensor m n -> Tensor m' n'
  tf f g = linear (\(x,y) -> te (f (basis x)) (g (basis y)))
  -- tf f g (decompose -> xs)
  --    = sum [k *> te (f (basis x)) (g (basis y)) | ((x,y),k) <- xs]

-- tfc :: (Scalar (Tensor (Tensor a a') (Tensor b b')) ~ Scalar (Tensor c c')) => (Tensor a b -> c) -> (Tensor a' b' -> c') -> Tensor (Tensor a a') (Tensor b b') -> Tensor c c'
-- tfc _ _ _ _ f g = linear (\((x,x'),(y,y')) -> te (f (te (basis x) (basis y))) (g (te (basis x') (basis y'))))


-- -- HasBasis => FreeModule
-- class (Functor f, Monad f) => FreeModule f where
--   unique :: (Module m) => (a -> f a) -> m -> (a -> m) -> f a -> m
--
-- instance (FreeModule f) => HasBasis (f b) where
--   type Basis (f b) = b
--
-- instance (FreeModule f) => Module (f b) where
--
-- data FreeM k :: * -> * where
--   Test :: a -> FreeM k a
--
-- instance Functor (FreeM k) where
--   fmap :: (k ~ Scalar (FreeM k a), k ~ Scalar (FreeM k b), Scalar (FreeM k a) ~ Scalar (FreeM k b),
--           HasBasis (FreeM k b), Module (FreeM k a),
--           HasBasis (FreeM k a), Module (FreeM k b), Basis (FreeM k a) ~ a,
--           Basis (FreeM k b) ~ b) => (a -> b) -> FreeM k a -> FreeM k b
--   fmap m (decompose -> xs) = sum [ c *> basis (m x) | (x,c) <- xs]
