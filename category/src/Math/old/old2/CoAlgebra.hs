{-# LANGUAGE
    MultiParamTypeClasses,
    TypeFamilies,
    ConstraintKinds,
    GADTs
#-}

module Math.CoAlgebra where

import Math.Module
import Math.Algebra

class CoAlgebra name m where
  counit :: name -> m -> Scalar m
  comult :: name -> m -> Tensor m m
