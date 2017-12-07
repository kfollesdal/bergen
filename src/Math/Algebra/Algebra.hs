-- TODO
-- @ Documentation
-- @ Make test

{-# LANGUAGE
    NoImplicitPrelude,
    MultiParamTypeClasses,
    FlexibleInstances,
    NamedFieldPuns,
    RecordWildCards
#-}

{-|
Module      : Math.Algebra
Description : Defenition of Algebras for Haskell.
Maintainer  : Kristoffer K. Føllesdal <kfollesdal@gmail.com>
-}

module Math.Algebra.Algebra where

import Math.Algebra.Module

data AlgebraD m = AlgebraD {
  unitD :: Scalar m -> m,
  multD :: Tensor m m -> m
  }

class (Module m) => Algebra name m where
  algebra :: name -> AlgebraD m
  unit :: name -> Scalar m -> m
  mult :: name -> Tensor m m -> m

instance (Module m) => Algebra (AlgebraD m) m where
  algebra = \x -> x
  unit (AlgebraD {unitD,..}) = unitD
  mult (AlgebraD {unitD,..}) = multD
