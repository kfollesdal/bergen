{-# LANGUAGE
    MultiParamTypeClasses,
    TypeFamilies
#-}

module Math.HopfAlgebra where


import Math.Bialgebra
import Math.Algebra
import Math.CoAlgebra

data HopfAlgebra2 bialgebra = HopfAlgebra2

data Hopfalgebra alg coalg m = Hopfalgebra {
  bialgebra :: Bialgebra alg coalg m,
  antipode :: m -> m
                               }

instance (Algebra alg m) => Algebra (Hopfalgebra alg coalg m) m where
  unit name = unit (algebra $ bialgebra name)
  mult name = mult (algebra $ bialgebra name)

instance (Bialgebra alg coalg m) => CoAlgebra (Hopfalgebra alg coalg m) where
  counit name = counit (coalgebra $ bialgebra name)
  comult name = comult (coalgebra $ bialgebra name)


-- BCK
-- MKW
-- Shuffel
-- CEM
-- PLS

-- 7 august tilbake
