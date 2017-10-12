{-# LANGUAGE
   NoImplicitPrelude,
   TypeFamilies,
   FlexibleInstances,
   FlexibleContexts,
   MultiParamTypeClasses,
   ViewPatterns
#-}

module Math.Module.FreeModule where

import Math.Monoid
import Math.Group
import Math.Module
import Math.Module.TensorProduct
import Math.Ring
--import Math.Algebra
import Text.Show
import GHC.Classes (Ord)
import Data.Eq
-- import Data.Maybe
import GHC.Base hiding (Module, (*>))
import Data.List (head, lookup)


-- class FreeModule

newtype FreeM k b = FM [(b,k)]

-- data FreeM r a where
--   FM :: (CommutativeRing r) => [(a,r)] -> FreeM r a

instance (Show k, Eq k, Show b) => Show (FreeM k b) where
    show (FM []) = "0"
    show (FM ts) = concatWithPlus $ map showTerm ts
        where showTerm (b,x) | show b == "1" = show x
                             | show x == "1" = show b
                             | show x == "-1" = "-" ++ show b
                             | otherwise = (if isAtomic (show x) then show x else "(" ++ show x ++ ")") ++
                                           show b
                                           -- (if ' ' `notElem` show b then show b else "(" ++ show b ++ ")")
                                           -- if we put this here we miss the two cases above
              concatWithPlus (t1:t2:ts) = if head t2 == '-'
                                          then t1 ++ concatWithPlus (t2:ts)
                                          else t1 ++ '+' : concatWithPlus (t2:ts)
              concatWithPlus [t] = t
              isAtomic (c:cs) = isAtomic' cs
              isAtomic' ('^':'-':cs) = isAtomic' cs
              isAtomic' ('+':cs) = False
              isAtomic' ('-':cs) = False
              isAtomic' (c:cs) = isAtomic' cs
              isAtomic' [] = True

zerov :: FreeM k b
zerov = FM []

add :: (Eq k, Ord b, AddidtativeMonoid k) => FreeM k b -> FreeM k b -> FreeM k b
add (FM ts) (FM us) = FM $ addmerge ts us where

-- addmerge :: (AddidtativeMonoid k, Eq k, Ord b) => [(b,k)] -> [(b,k)] -> [(b,k)]
  addmerge ((a,x):ts) ((b,y):us) =
    case compare a b of
      LT -> (a,x) : addmerge ts ((b,y):us)
      EQ -> if x+y == zero then addmerge ts us else (a,x+y) : addmerge ts us
      GT -> (b,y) : addmerge ((a,x):ts) us
  addmerge ts [] = ts
  addmerge [] us = us

negatev :: (Eq k, AbelianGroup k) => FreeM k b -> FreeM k b
negatev (FM ts) = FM $ map (\(b,x) -> (b,negate x)) ts

smultL :: (CommutativeRing k, Eq k) => k -> FreeM k b -> FreeM k b
smultL k (FM ts) | k == zero = zerov
                 | otherwise = FM [(ei,k*xi) | (ei,xi) <- ts]


instance (CommutativeRing r, Eq r, Ord b) => Module (FreeM r b) where
  type Scalar (FreeM r b) = r
  (*>) = smultL

instance (Eq k, CommutativeRing k, Ord b) => AbelianGroup (FreeM k b) where
  negate = negatev

instance (CommutativeRing k ,Ord b, Eq k) => AddidtativeMonoid (FreeM k b) where
  zero = zerov
  (+) = add

instance (CommutativeRing k, Ord b, Eq k) => HasBasis (FreeM k b) where
  type Basis (FreeM k b) = b
  basisValue x = FM [(x,u)]
  decompose (FM xs) = xs
  decompose' x b = case lookup b $ decompose x of
                     Nothing -> zero
                     Just c -> c
  recompose xs = FM xs

instance (CommutativeRing k, Ord b, Eq k, Ord a) => HasTensorProduct (FreeM k b) (FreeM k a) where
  type Tensor (FreeM k b) (FreeM k a) = FreeM k (b,a)

-- type instance Tensor (FreeM k a) (FreeM k b) = FreeM k (a,b)

-- te :: (MultiplicativeMonoid k) => FreeM k a -> FreeM k b -> Tensor (FreeM k a) (FreeM k b)
-- te (FM xs) (FM ys) = FM [((a,b),x*y) | (a,x) <- xs , (b,y) <-ys]
--
-- tf :: (Ord a, Ord a', Ord b', Ord b, CommutativeRing k, Eq k) => (FreeM k a -> FreeM k a') -> (FreeM k b -> FreeM k b')
--   -> Tensor (FreeM k a) (FreeM k b) -> Tensor (FreeM k a') (FreeM k b')
-- tf f g (FM ts) = msum [ x *> te (f $ basisValue a) (g $ basisValue b) | ((a,b),x) <- ts]
