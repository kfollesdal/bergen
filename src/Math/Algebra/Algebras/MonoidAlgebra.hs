-- STARTED not added to project

{-# LANGUAGE
  NoImplicitPrelude
#-}

module Math.Algebra.Algebras.MonoidAlgebra where

import Math.Algebra.Algebra
import Math.Algebra.Monoid
import Math.Algebra.Ring
import Math.Algebra.Module
import Math.Algebra.Module.FreeModule
import GHC.Base (Eq (..), Ord (..), (++))

import Math.Algebra.Coalgebra
import Math.Algebra.Bialgebra
import Math.Algebra.HopfAlgebra

monoidAlg :: (MultiplicativeMonoid b, CommutativeRing k, Eq k, Ord b) => AlgebraD (FModule k b)
monoidAlg = AlgebraD {
  unitD = \x -> x *> i u,
  multD = linear (\(x,y) -> i (x*y))
}

cotest :: (CommutativeRing k, Ord b, Eq k) => CoalgebraD (FModule k b)
cotest = CoalgebraD {
  counitD = \x -> u,
  comultD = linear (\x -> te_ x x)
}

bitest :: (CommutativeRing k, MultiplicativeMonoid b, Ord b, Eq k) => BialgebraD (FModule k b)
bitest = BialgebraD {
  algebraD = monoidAlg,
  coalgebraD = cotest
}

hopftest :: (CommutativeRing k, MultiplicativeMonoid b, Ord b, Eq k) => HopfAlgebraD (FModule k b)
hopftest = HopfAlgebraD {
  bialgebraD = bitest,
  antipodeD = \x -> x
}


-- graftAlg :: (CommutativeRing k) => AlgebraD (ForrestModule k b)
-- gaftAlg = AlgebraD {
--   unitD = ,
--   multD = graft
-- }
