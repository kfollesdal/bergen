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

instance (Module m, HasTensorProduct m m) => Algebra (BialgebraD m) m where
  algebra = algebraD
  unit = unitD . algebraD
  mult = multD . algebraD

instance (Module m, HasTensorProduct m m) => CoAlgebra (BialgebraD m) m where
  coalgebra = coalgebraD
  counit = counitD . coalgebraD
  comult = comultD . coalgebraD
