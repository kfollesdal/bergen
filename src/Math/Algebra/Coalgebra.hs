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
Module      : Math.Coalgebra
Description : Defenition of Coalgebras for Haskell.
Maintainer  : Kristoffer K. Føllesdal <kfollesdal@gmail.com>
-}

module Math.Algebra.Coalgebra where

import Math.Algebra.Module

data CoalgebraD m = CoalgebraD {
  counitD :: m -> Scalar m,
  comultD :: m -> Tensor m m
  }

class (Module m) => Coalgebra name m where
  coalgebra :: name -> CoalgebraD m
  counit :: name -> m -> Scalar m
  comult :: name -> m -> Tensor m m

instance (Module m) => Coalgebra (CoalgebraD m) m where
  coalgebra = \x -> x
  counit (CoalgebraD {counitD,..}) = counitD
  comult (CoalgebraD {comultD,..}) = comultD
