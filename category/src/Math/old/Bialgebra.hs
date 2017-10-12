{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    NamedFieldPuns,
    RecordWildCards
#-}

module Math.Bialgebra where

import Math.Module
import Math.Module.TensorProduct
import Math.Algebra
import Math.CoAlgebra

-- temp imports [Tensor problem]
-- import Math.Module.FreeModule
-- import Math.Ring

data BialgebraD m = BialgebraD {
  algebraD :: AlgebraD m,
  coalgebraD :: CoAlgebraD m
  }

class (Algebra name m, CoAlgebra name m, HasTensorProduct m m) => Bialgebra name m where
  bialgebra :: name -> BialgebraD m
  convolution :: name -> (m -> m) -> (m -> m) -> m -> m
  convolution name f g = mult name . tf f g . comult name

instance (HasTensorProduct m m) => Bialgebra (BialgebraD m) m where
  bialgebra = id

-- instance (CommutativeRing k, Ord b, Eq k) => Bialgebra (BialgebraD (FreeM k b)) (FreeM k b) where
--   bialgebra = id
--   convolution (BialgebraD {algebraD = AlgebraD {multD,..}, coalgebraD = CoAlgebraD {comultD,..}}) f g = multD . tf f g . comultD

instance (Module m, HasTensorProduct m m) => Algebra (BialgebraD m) m where
  algebra = algebraD
  unit = unitD . algebraD
  mult = multD . algebraD
  -- algebra (BialgebraD {algebraD,..}) = algebraD
  -- unit (BialgebraD {algebraD = AlgebraD {unitD, ..},..}) = unitD
  -- mult (BialgebraD {algebraD = AlgebraD {multD, ..},..}) = multD

instance (Module m, HasTensorProduct m m) => CoAlgebra (BialgebraD m) m where
  coalgebra = coalgebraD
  counit = counitD . coalgebraD
  comult = comultD . coalgebraD
  -- coalgebra (BialgebraD {coalgebraD,..}) = coalgebraD
  -- counit (BialgebraD {coalgebraD = CoAlgebraD {counitD, ..},..}) = counitD
  -- comult (BialgebraD {coalgebraD = CoAlgebraD {comultD, ..},..}) = comultD
