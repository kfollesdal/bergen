{-# LANGUAGE
    NoImplicitPrelude,
    ViewPatterns,
    TypeFamilies,
    MultiParamTypeClasses,
    AllowAmbiguousTypes,
    FlexibleContexts
#-}

module Math.Module.TensorProduct where

import Math.Module
import Math.Monoid
-- import Math.Module.FreeModule
import Math.Ring
import GHC.Classes

class (Module m, Module n,
      Scalar m ~ Scalar n, Scalar m ~ Scalar (Tensor m n),
      Basis (Tensor m n) ~ (Basis m, Basis n),
      HasBasis m, HasBasis n, HasBasis (Tensor m n))
      => HasTensorProduct m n where
  type Tensor m n :: *
  -- te_ :: (Basis (Tensor m n) ~ (Basis m, Basis n)) => Basis m -> Basis n -> Tensor m n
  -- te_ x y = basisValue (x,y)
  -- te'2 :: (Tensor (Basis m) (Basis n) ~ Tensor m n, Basis (Tensor m n) ~ (Basis m, Basis n)) => m -> n -> Tensor m  n
  -- te'2 (decompose -> xs) (decompose -> ys) = msum [(a*b) *> te_ x y| (x,a) <-xs, (y,b) <- ys]
  te :: m -> n -> Tensor m n
  te (decompose -> xs) (decompose -> ys) = recompose [((x,y),a*b) | (x,a) <-xs, (y,b) <- ys]
  tf :: (Scalar (Tensor m n) ~ Scalar (Tensor m' n'), HasTensorProduct m' n')
      => (m -> m') -> (n -> n') -> Tensor m n -> Tensor m' n'
  tf f g (decompose -> xs) = msum [k *> te (f (basisValue x)) (g (basisValue y)) | ((x,y),k) <- xs]


-- te2 :: (HasBasis m, HasBasis n, HasBasis l,
--        Scalar m ~ Scalar n, Scalar l ~ Scalar m,
--        Basis l ~ (Basis m, Basis n))
--        => m -> n -> l
-- te2 (decompose -> xs) (decompose -> ys) = recompose [((x,y),a*b) |(x,a) <- xs, (y,b) <- ys]
--
-- te3 :: (HasBasis m, HasBasis n, HasBasis (Tensor m n),
--        Scalar m ~ Scalar n, Scalar (Tensor m n) ~ Scalar m,
--        Basis (Tensor m n) ~ (Basis m, Basis n))
--        => m -> n -> Tensor m n
-- te3 (decompose -> xs) (decompose -> ys) = recompose [((x,y),a*b) |(x,a) <- xs, (y,b) <- ys]
--
-- tf3 :: (HasBasis (Tensor q w), HasBasis (Tensor q' w'),
--         Basis (Tensor q w)~ (Basis q, Basis w),
--         Basis (Tensor q' w')~ (Basis q', Basis w'),
--         m ~ q, m'~q', n~w, n'~w',
--         Scalar (Tensor q w) ~ Scalar (Tensor q' w'),
--         Tensor (Basis q) (Basis w) ~ Tensor q' w',
--         Scalar (Basis q) ~ Scalar (Basis w),
--         Scalar (Basis q') ~ Scalar (Basis w'),
--         Basis (Basis q) ~ Basis q',
--         Scalar (Tensor q' w') ~ Scalar (Basis w),
--         Basis (Basis w) ~ Basis w',
--         HasBasis q, (HasBasis (Basis q)),
--         (HasBasis (Basis w))  )
--         => (m -> m') -> (n-> n') -> Tensor q w -> Tensor q' w'
-- tf3 f g (decompose -> xs) =  msum [k *> (te3 x y)|((x,y),k) <- xs]
