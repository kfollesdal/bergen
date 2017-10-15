-- TODO
-- @ Documentation
-- @ Make tests
-- @ Take a look at the show function.

{-# LANGUAGE
   NoImplicitPrelude,
   TypeFamilies,
   MultiParamTypeClasses
#-}

{-|
Module      : Math.Module.FreeModule
Description : Implementation of Free Module in Haskell.
Maintainer  : Kristoffer K. Føllesdal <kfo021@uib.no>
-}

module Math.Module.FreeModule where

import Math.Monoid
import Math.Group
import Math.Module
import Math.Ring
import Text.Show (Show (..))
import GHC.Base (Eq (..), Ord (..), Ordering (..), Maybe (..), ($), (++), Bool (..), map, otherwise)
import Data.List (head, lookup)

newtype FreeModule k b = FM [(b,k)]

instance (Show k, Eq k, Show b) => Show (FreeModule k b) where
    show (FM []) = "0"
    show (FM ts) = concatWithPlus $ map showTerm ts
        where showTerm (b,x) | show b == "1" = show x
                             | show x == "1" = show b
                             | show x == "-1" = "-" ++ show b
                             | otherwise = (if isAtomic (show x) then show x else "(" ++ show x ++ ")") ++ show b
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

instance (CommutativeRing k ,Ord b, Eq k) => AddidtativeMonoid (FreeModule k b) where
  zero = zerov
  (+) = add

zerov :: FreeModule k b
zerov = FM []

add :: (Eq k, Ord b, AddidtativeMonoid k) => FreeModule k b -> FreeModule k b -> FreeModule k b
add (FM ts) (FM us) = FM $ addmerge ts us where
  addmerge ((a,x):ts) ((b,y):us) =
    case compare a b of
      LT -> (a,x) : addmerge ts ((b,y):us)
      EQ -> if x+y == zero then addmerge ts us else (a,x+y) : addmerge ts us
      GT -> (b,y) : addmerge ((a,x):ts) us
  addmerge ts [] = ts
  addmerge [] us = us

instance (Eq k, CommutativeRing k, Ord b) => AbelianGroup (FreeModule k b) where
  negate = negatev

negatev :: (Eq k, AbelianGroup k) => FreeModule k b -> FreeModule k b
negatev (FM ts) = FM $ map (\(b,x) -> (b,negate x)) ts

instance (CommutativeRing r, Eq r, Ord b) => Module (FreeModule r b) where
  type Scalar (FreeModule r b) = r
  (*>) = smultL

smultL :: (CommutativeRing k, Eq k) => k -> FreeModule k b -> FreeModule k b
smultL k (FM ts) | k == zero = zerov
                 | otherwise = FM [(ei,k*xi) | (ei,xi) <- ts]

instance (CommutativeRing k, Ord b, Eq k) => HasBasis (FreeModule k b) where
  type Basis (FreeModule k b) = b
  basis x = FM [(x,u)]
  decompose (FM xs) = xs
  decompose' x b = case lookup b $ decompose x of
                     Nothing -> zero
                     Just c -> c
  linearCombi xs = FM xs

instance (CommutativeRing k, Ord b, Eq k, Ord a) => HasTensorProduct (FreeModule k b) (FreeModule k a) where
  type Tensor (FreeModule k b) (FreeModule k a) = FreeModule k (b,a)
