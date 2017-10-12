-- # TODO
-- * Documentation

{-# LANGUAGE
    FlexibleInstances,
    UndecidableInstances
#-}

{-|
Module      : Math.Ring
Description : Defenition of Ring for Haskell.
Maintainer  : Kristoffer K. Føllesdal <kfo021@uib.no>
-}

module Math.Ring  (
  -- * Rings
  Ring (..),
  CommutativeRing (..)
) where

import Math.Monoid
import Math.Group

class (AbelianGroup r, MultiplicativeMonoid r) => Ring r where

class (AbelianGroup r, CommutativMonoid r) => CommutativeRing r where

instance CommutativeRing Integer

instance CommutativeRing Int
