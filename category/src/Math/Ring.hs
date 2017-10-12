{-# LANGUAGE
    FlexibleInstances,
    UndecidableInstances
#-}

module Math.Ring where

import Math.Monoid
import Math.Group
import GHC.Num as N

class (AbelianGroup r, MultiplicativeMonoid r) => Ring r where

class (AbelianGroup r, CommutativMonoid r) => CommutativeRing r where

instance CommutativeRing Integer

instance CommutativeRing Int
