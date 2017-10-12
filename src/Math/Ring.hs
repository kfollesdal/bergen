{-# LANGUAGE
    FlexibleInstances,
    UndecidableInstances
#-}

module Math.Ring where

import Math.Monoid
import Math.Group

class (AbelianGroup r, MultiplicativeMonoid r) => Ring r where

class (AbelianGroup r, CommutativMonoid r) => CommutativeRing r where

instance CommutativeRing Integer

instance CommutativeRing Int
