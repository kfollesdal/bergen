-- TODO
-- @ Documentation
-- @ Add tests to ModuleTest.hs
-- Left, Rigth, bimodule?

{-# LANGUAGE
  NoImplicitPrelude,
  TypeFamilies,
  FlexibleContexts
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
  HasBasis (..)
) where

import Math.Ring (CommutativeRing)
import Math.Group (AbelianGroup)

class (AbelianGroup m, CommutativeRing (Scalar m)) => Module m where
  type Scalar m :: *
  (*>) :: Scalar m -> m -> m

class (Module m) => HasBasis m where
  type Basis m :: *
  basis :: Basis m -> m
  decompose :: m -> [(Basis m, Scalar m)]
  decompose' :: m -> Basis m -> Scalar m
  linearCombi :: [(Basis m, Scalar m)] -> m
