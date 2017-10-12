{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    NamedFieldPuns,
    RecordWildCards
#-}

module Math.Algebra where

import Math.Module
import Math.Module.TensorProduct

data AlgebraD m = AlgebraD {
  unitD :: Scalar m -> m,
  multD :: Tensor m m -> m
  }

class (Module m, HasTensorProduct m m) => Algebra name m where
  algebra :: name -> AlgebraD m
  unit :: name -> Scalar m -> m
  mult :: name -> Tensor m m -> m

instance (Module m, HasTensorProduct m m) => Algebra (AlgebraD m) m where
  algebra = id
  unit (AlgebraD {unitD,..}) = unitD
  mult (AlgebraD {unitD,..}) = multD
