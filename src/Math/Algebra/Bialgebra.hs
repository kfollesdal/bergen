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
Module      : Math.Bialgebra
Description : Defenition of Bialgebras for Haskell.
Maintainer  : Kristoffer K. Føllesdal <kfollesdal@gmail.com>
-}

module Math.Algebra.Bialgebra where

import Math.Algebra.Module
import Math.Algebra.Algebra
import Math.Algebra.Coalgebra
import GHC.Base ((.))

data BialgebraD m = BialgebraD {
  algebraD :: AlgebraD m,
  coalgebraD :: CoalgebraD m
  }

class (Algebra name m, Coalgebra name m, HasTensorProduct m m) => Bialgebra name m where
  bialgebra :: name -> BialgebraD m
  convolution :: name -> (m -> m) -> (m -> m) -> m -> m
  convolution name f g = mult name . tf f g . comult name

instance (Module m, HasTensorProduct m m) => Bialgebra (BialgebraD m) m where
  bialgebra = \x -> x

instance (Module m) => Algebra (BialgebraD m) m where
  algebra = algebraD
  unit = unitD . algebraD
  mult = multD . algebraD

instance (Module m) => Coalgebra (BialgebraD m) m where
  coalgebra = coalgebraD
  counit = counitD . coalgebraD
  comult = comultD . coalgebraD
