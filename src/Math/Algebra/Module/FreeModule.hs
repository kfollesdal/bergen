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
Maintainer  : Kristoffer K. Føllesdal <kfollesdal@gmail.com>
-}

module Math.Algebra.Module.FreeModule (
  FModule (..)
) where

import Math.Algebra.Monoid
import Math.Algebra.Group
import Math.Algebra.Module
import Math.Algebra.Ring
import Text.Show (Show (..))
import GHC.Base (Eq (..), Ord (..), Functor(..), (.), Ordering (..), Maybe (..), ($), (++), Bool (..), map, otherwise)
import Data.List (head, lookup)

newtype FModule k b = FM [(b,k)]

instance (Show k, Eq k, Show b) => Show (FModule k b) where
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

instance (CommutativeRing k ,Ord b, Eq k) => AddidtativeMonoid (FModule k b) where
  zero = zerov
  (+) = add

zerov :: FModule k b
zerov = FM []

add :: (Eq k, Ord b, AddidtativeMonoid k) => FModule k b -> FModule k b -> FModule k b
add (FM ts) (FM us) = FM $ addmerge ts us where
  addmerge ((a,x):ts) ((b,y):us) =
    case compare a b of
      LT -> (a,x) : addmerge ts ((b,y):us)
      EQ -> if x+y == zero then addmerge ts us else (a,x+y) : addmerge ts us
      GT -> (b,y) : addmerge ((a,x):ts) us
  addmerge ts [] = ts
  addmerge [] us = us

instance (Eq k, CommutativeRing k, Ord b) => AbelianGroup (FModule k b) where
  negate = negatev

negatev :: (Eq k, AbelianGroup k) => FModule k b -> FModule k b
negatev (FM ts) = FM $ map (\(b,x) -> (b,negate x)) ts

instance (CommutativeRing r, Eq r, Ord b) => Module (FModule r b) where
  type Scalar (FModule r b) = r
  (*>) = smultL

smultL :: (CommutativeRing k, Eq k) => k -> FModule k b -> FModule k b
smultL k (FM ts) | k == zero = zerov
                 | otherwise = FM [(ei,k*xi) | (ei,xi) <- ts]

instance Functor (FModule k) where
  fmap m  (FM xs)= FM $ map (\(b,c) -> (m b, c)) xs

instance (MultiplicativeMonoid k) => FreeModule (FModule k) where
  i x = FM [(x,u)]
  linear f (FM xs) = sum [c *> (f x) | (x,c) <- xs]
  decompose (FM xs) = xs
  compose xs = FM xs
