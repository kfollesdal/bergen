{-# LANGUAGE
    NoImplicitPrelude,
    ScopedTypeVariables,
    GADTs,
    KindSignatures,
    TypeFamilies,
    TypeOperators
#-}

module Math.Module.Category where

import Math.Module
import Data.Category
import Data.Category.Limit
import Data.Category.Functor
import Data.Category.Product
import Math.Module.FreeModule
import Math.Algebra (Tensor)

newtype Linear k a b = Linear (FreeM k a -> FreeM k b)

instance Category (Linear k) where
  src (Linear (_:: FreeM k a -> FreeM k b)) = Linear (\x -> x)
  tgt (Linear (_:: FreeM k a -> FreeM k b)) = Linear (\x -> x)
  Linear f . Linear g = Linear (f . g)

class Category k => HasBinaryProducts2 k where
  type BinaryProduct2 (k :: * -> * -> *) x y :: *

  proj12 :: Obj k x -> Obj k y -> k (BinaryProduct2 k x y) x
  -- proj2 :: Obj k x -> Obj k y -> k (BinaryProduct k x y) y

  -- (&&&) :: (k a x) -> (k a y) -> (k a (BinaryProduct k x y))

  -- (***) :: (k a1 b1) -> (k a2 b2) -> (k (BinaryProduct k a1 a2) (BinaryProduct k b1 b2))
  -- l *** r = (l . proj1 (src l) (src r)) &&& (r . proj2 (src l) (src r))

instance HasBinaryProducts2 (Linear k) where
  type BinaryProduct2 (Linear k) a b = Tensor (FreeM k a) (FreeM k b)

  proj12 (Linear (_ :: FreeM k a -> FreeM k a)) (Linear (_:: FreeM k b -> FreeM k b)) = Linear fun where
    fun :: BinaryProduct2 (Linear k) (FreeM k a) (FreeM k b) -> (FreeM k a)
    fun (FM xs) = FM [(x,k) | ((x,y),k) <- xs]
  -- proj2 :: Obj k x -> Obj k y -> k (BinaryProduct k x y) y

  -- (&&&) :: (k a x) -> (k a y) -> (k a (BinaryProduct k x y))

  -- (***) :: (k a1 b1) -> (k a2 b2) -> (k (BinaryProduct k a1 a2) (BinaryProduct k b1 b2))
  -- l *** r = (l . proj1 (src l) (src r)) &&& (r . proj2 (src l) (src r))
