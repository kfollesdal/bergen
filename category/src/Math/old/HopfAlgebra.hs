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
-- -- temp imports Tensor problem
-- import Math.Ring
-- import Math.Module.FreeModule

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
  -- algebra (HopfAlgebraD {bialgebraD = BialgebraD {algebraD,..},..}) = algebraD
  -- unit (HopfAlgebraD {bialgebraD = BialgebraD {algebraD = AlgebraD {unitD,..},..},..}) = unitD
  -- mult (HopfAlgebraD {bialgebraD = BialgebraD {algebraD = AlgebraD {multD,..},..},..}) = multD

instance (Module m, HasTensorProduct m m) => CoAlgebra (HopfAlgebraD m) m where
  coalgebra = coalgebra . bialgebraD
  counit = counit . bialgebraD
  comult = comult . bialgebraD
  -- coalgebra (HopfAlgebraD {bialgebraD = BialgebraD {coalgebraD,..},..}) = coalgebraD
  -- counit (HopfAlgebraD {bialgebraD = BialgebraD {coalgebraD = CoAlgebraD {counitD,..},..},..}) = counitD
  -- comult (HopfAlgebraD {bialgebraD = BialgebraD {coalgebraD = CoAlgebraD {comultD,..},..},..}) = comultD

instance (Module m, HasTensorProduct m m) => Bialgebra (HopfAlgebraD m) m where
  bialgebra = bialgebraD
  -- bialgebra (HopfAlgebraD {bialgebraD,..}) = bialgebraD

-- instance (Ord b, CommutativeRing k, Eq k) => Bialgebra (HopfAlgebraD (FreeM k b)) (FreeM k b) where
--   bialgebra (HopfAlgebraD {bialgebraD,..}) = bialgebraD
--   convolution (HopfAlgebraD {bialgebraD = BialgebraD {algebraD = AlgebraD {multD,..}, coalgebraD = CoAlgebraD {comultD,..}},..}) f g = multD . tf f g . comultD
