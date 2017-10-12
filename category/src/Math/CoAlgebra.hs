{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    NamedFieldPuns,
    RecordWildCards
#-}

module Math.CoAlgebra where

import Math.Module
import Math.Module.TensorProduct

data CoAlgebraD m = CoAlgebraD {
  counitD :: m -> Scalar m,
  comultD :: m -> Tensor m m
  }

class (Module m, HasTensorProduct m m) => CoAlgebra name m where
  coalgebra :: name -> CoAlgebraD m
  counit :: name -> m -> Scalar m
  comult :: name -> m -> Tensor m m

instance (Module m, HasTensorProduct m m) => CoAlgebra (CoAlgebraD m) m where
  coalgebra = id
  counit (CoAlgebraD {counitD,..}) = counitD
  comult (CoAlgebraD {comultD,..}) = comultD
