-- TODO
-- @ Documentation
-- @ Make test

{-# LANGUAGE
    MultiParamTypeClasses,
    TypeFamilies,
    FlexibleInstances,
    NamedFieldPuns,
    RecordWildCards,
    UndecidableInstances
#-}

{-|
Module      : Math.HopfAlgebra
Description : Defenition of Hopf Algebras for Haskell.
Maintainer  : Kristoffer K. Føllesdal <kfollesdal@gmail.com>
-}

module Math.Algebra.HopfAlgebra where

import Math.Algebra.Module
import Math.Algebra.Algebra
import Math.Algebra.Coalgebra
import Math.Algebra.Bialgebra

data HopfAlgebraD m = HopfAlgebraD {
  bialgebraD :: BialgebraD m,
  antipodeD :: m -> m
  }

class HopfAlgebra name m where
  hopfAlgebra :: name -> HopfAlgebraD m
  antipode :: name -> m -> m

instance (Module m, HasTensorProduct m m, m ~ n) => HopfAlgebra (HopfAlgebraD m) n where
  hopfAlgebra = \x -> x
  antipode (HopfAlgebraD {antipodeD,..}) = antipodeD

instance (Module m, HasTensorProduct m m, m ~ n) => Algebra (HopfAlgebraD m) n where
  algebra = algebra . bialgebraD
  unit = unit . bialgebraD
  mult = mult . bialgebraD

instance (Module m, HasTensorProduct m m, m ~ n) => Coalgebra (HopfAlgebraD m) n where
  coalgebra = coalgebra . bialgebraD
  counit = counit . bialgebraD
  comult = comult . bialgebraD

instance (Module m, HasTensorProduct m m, m ~ n) => Bialgebra (HopfAlgebraD m) n where
  bialgebra = bialgebraD
