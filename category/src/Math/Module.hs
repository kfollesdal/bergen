{-# LANGUAGE
  NoImplicitPrelude,
  MultiParamTypeClasses,
  TypeFamilies,
  FlexibleContexts,
  FlexibleInstances
#-}

module Math.Module where

import Math.Ring
import Math.Group

class (AbelianGroup m, CommutativeRing (Scalar m)) => Module m where
  type Scalar m :: *
  -- (*>) :: (CommutativeRing (Scalar m)) => Scalar m -> m -> m
  (*>) :: Scalar m -> m -> m

class (Module m) => HasBasis m where
  type Basis m :: *
  basisValue :: Basis m -> m
  decompose :: m -> [(Basis m, Scalar m)]
  decompose' :: m -> Basis m -> Scalar m
  recompose :: [(Basis m, Scalar m)] -> m


-- type family Tensor a b :: *





-- linearCombo :: [(m, Scalar m)] -> m

-- recompose :: [(Basis m, Scalar m)] -> m

-- class (Module2 m) => HasTensorProduct m where
--   type Tensor m m :: *

-- -- tf :: (a -> a') -> (b -> b') -> (Tensor a b) -> (Tensor a' b')
-- -- tf f g =

-- -- te :: m -> n -> Tensor m n
-- -- te x y =








-- What about left, right and bimodule?
-- class (CommutativeRing r, AbelianGroup m) => Module r m where
--   (*>) :: r -> m -> m
