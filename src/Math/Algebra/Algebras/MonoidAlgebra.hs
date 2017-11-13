-- STARTED not added to project

{-# LANGUAGE
  NoImplicitPrelude
#-}

module Math.Algebra.Algebra.MonoidAlgebra where

import Math.Algebra
import Math.Monoid
import Math.Ring
import Math.Module
import Math.Module.FreeModule
import GHC.Base (Eq (..), Ord (..))

monoidAlge :: (MultiplicativeMonoid b, CommutativeRing k, Eq k, Ord b) => AlgebraD (FreeModule k b)
monoidAlgebra = AlgebraD {
  unitD = \x -> x *> basis u,
  multD = linear (\(x,y) -> basis (x*y))
}

graftAlg :: (CommutativeRing k) => AlgebraD (ForrestModule k b)
gaftAlg = AlgebraD {
  unitD = ,
  multD = graft
}
