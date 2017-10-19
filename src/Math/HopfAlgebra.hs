-- TODO
-- @ Documentation
-- @ Make test

{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    NamedFieldPuns,
    RecordWildCards
#-}

{-|
Module      : Math.HopfAlgebra
Description : Defenition of Hopf Algebras for Haskell.
Maintainer  : Kristoffer K. Føllesdal <kfollesdal@gmail.com>
-}

module Math.HopfAlgebra where

import Math.Module
import Math.Algebra
import Math.Coalgebra
import Math.Bialgebra

data HopfAlgebraD m = HopfAlgebraD {
  bialgebraD :: BialgebraD m,
  antipodeD :: m -> m
  }

class HopfAlgebra name m where
  hopfAlgebra :: name -> HopfAlgebraD m
  antipode :: name -> m -> m

instance (Module m, HasTensorProduct m m) => HopfAlgebra (HopfAlgebraD m) m where
  hopfAlgebra = \x -> x
  antipode (HopfAlgebraD {antipodeD,..}) = antipodeD

instance (Module m, HasTensorProduct m m) => Algebra (HopfAlgebraD m) m where
  algebra = algebra . bialgebraD
  unit = unit . bialgebraD
  mult = mult . bialgebraD

instance (Module m, HasTensorProduct m m) => Coalgebra (HopfAlgebraD m) m where
  coalgebra = coalgebra . bialgebraD
  counit = counit . bialgebraD
  comult = comult . bialgebraD

instance (Module m, HasTensorProduct m m) => Bialgebra (HopfAlgebraD m) m where
  bialgebra = bialgebraD
