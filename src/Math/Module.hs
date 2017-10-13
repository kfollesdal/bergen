-- TODO
-- @ Documentation
-- @ Add tests to ModuleTest.hs
-- Left, Rigth, bimodule?

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
Maintainer  : Kristoffer K. Føllesdal <kfo021@uib.no>
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

class (Module m, Module n,
      Scalar m ~ Scalar n, Scalar m ~ Scalar (Tensor m n),
      Basis (Tensor m n) ~ (Basis m, Basis n),
      HasBasis m, HasBasis n, HasBasis (Tensor m n))
      => HasTensorProduct m n where
  type Tensor m n :: *
  te :: m -> n -> Tensor m n
  te (decompose -> xs) (decompose -> ys) = linearCombi [((x,y),a*b) | (x,a) <-xs, (y,b) <- ys]
  tf :: (Scalar (Tensor m n) ~ Scalar (Tensor m' n'), HasTensorProduct m' n')
      => (m -> m') -> (n -> n') -> Tensor m n -> Tensor m' n'
  tf f g (decompose -> xs) = msum [k *> te (f (basis x)) (g (basis y)) | ((x,y),k) <- xs]
