{-# LANGUAGE
    MultiParamTypeClasses,
    RankNTypes,
    FlexibleInstances,
    TypeFamilies
#-}


module Math.Bialgebra where

import Math.Algebra
import Math.CoAlgebra
import Math.Module






data Bialgebra alg coalg m = Bialgebra {
  algebra :: (Algebra alg m) => alg,
  coalgebra :: (CoAlgebra coalg m) => coalg
                                  }



-- class (Module2 m) => BialgebraC name m where
--   algebraC :: name -> Algebra alg m
--   convolution :: name -> (Linear m m) -> (Linear m m) -> (Linear m m)
--   convolution tag f g = mult tag . tf f g . comult tag

instance (Algebra alg a, CoAlgebra coalg a, a ~ b) => Algebra (Bialgebra alg coalg a) b where
  unit name = unit (algebra name)
  mult name = mult (algebra name)

instance (Algebra alg a, CoAlgebra coalg a, a~b) => CoAlgebra (Bialgebra alg coalg a) b where
  counit name = counit (coalgebra name)
  comult name = comult (coalgebra name)













-- class (Algebra alg m, CoAlgebra coalg m) => Bialgebra name alg coalg m where
--   algebra :: name -> alg
--   coalgebra :: name coalg

-- instance Algebra (Bialgebra name alg coalg m) where
--   unit _ = unit alg
--   mult _ = mult alg

-- data BialgebraD m = BialgebraD {
--   algebra :: AlgebraD m--,
--   --coalgebra :: CoAlgebra m
--                              }

-- instance (Module2 m) => Algebra (BialgebraD m) m where
--   unit (BialgebraD {algebra = AlgebraD {unitD,..},..}) = unitD
--   mult (BialgebraD {algebra = AlgebraD {multD,..},..}) = multD


