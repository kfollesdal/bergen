{-# LANGUAGE
    TypeFamilies,
    MultiParamTypeClasses,
    GADTs,
    RankNTypes,
    DuplicateRecordFields,
    NamedFieldPuns,
    RecordWildCards,
    FlexibleInstances,
    FlexibleContexts
#-}

module Math.Algebra where

import Math.Module

-- class HasTensorProduct

data AlgebraName = AlgebraName

-- data AlgebraD name m = AlgebraD


class (Module m) => Algebra name m where
  unit :: name -> Scalar m -> m
  mult :: name -> Tensor m m -> m


















-- data AlgebraD m = AlgebraD {
--   unitD :: (Module m) => Scalar m -> m,
--   multD :: Tensor m m -> m
-- }

-- instance (Module m, a~m) => Algebra (AlgebraD a) m where
--   unit (AlgebraD {unitD,..}) = unitD
--   mult name = multD where
--     AlgebraD {multD,..} = name




-- data Bialgebra m where
--   Bialgebra :: {algebra :: Algebra m, coalgebra :: Coalgebra m} -> Bialgebra m



