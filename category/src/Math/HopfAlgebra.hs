{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    NamedFieldPuns,
    RecordWildCards
#-}

module Math.HopfAlgebra where

import Math.Module
import Math.Module.TensorProduct
import Math.Algebra
import Math.CoAlgebra
import Math.Bialgebra

data HopfAlgebraD m = HopfAlgebraD {
  bialgebraD :: BialgebraD m,
  antipodeD :: m -> m
  }

class HopfAlgebra name m where
  hopfAlgebra :: name -> HopfAlgebraD m
  antipode :: name -> m -> m

instance (Module m, HasTensorProduct m m) => HopfAlgebra (HopfAlgebraD m) m where
  hopfAlgebra = id
  antipode (HopfAlgebraD {antipodeD,..}) = antipodeD

instance (Module m, HasTensorProduct m m) => Algebra (HopfAlgebraD m) m where
  algebra = algebra . bialgebraD
  unit = unit . bialgebraD
  mult = mult . bialgebraD

instance (Module m, HasTensorProduct m m) => CoAlgebra (HopfAlgebraD m) m where
  coalgebra = coalgebra . bialgebraD
  counit = counit . bialgebraD
  comult = comult . bialgebraD

instance (Module m, HasTensorProduct m m) => Bialgebra (HopfAlgebraD m) m where
  bialgebra = bialgebraD
