{-# LANGUAGE
  MultiParamTypeClasses,
  FlexibleInstances,
  FlexibleContexts,
  TypeApplications,
  ScopedTypeVariables
#-}

{-|
Module      : Algebra.Monoid.Test
Description : Short description
Maintainer  : kfo021@uib.no

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Algebra.Monoid.Test where

import Prelude hiding (Monoid, negate, (+))
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Data.List
import Test.Tasty.Options

import Test.SmallCheck.Series
import Control.Monad.Identity

class Monoid a where
  op :: a -> a-> a
  e :: a

instance Monoid [a] where
  op = (++)
  e = []

left_identity :: (Monoid m, Eq m) => m -> Bool
left_identity x = op e x == x

right_identity :: (Monoid m, Eq m) => m -> Bool
right_identity x = op x e == x

identity :: (Monoid m, Eq m) => m -> Bool
identity x = left_identity x && right_identity x

assosiativ :: (Monoid m, Eq m) => m -> m -> m -> Bool
assosiativ x y z = op x (op y z) == op (op x y) z

data Type a = T

test_monoid :: forall a . (Monoid a, Eq a, Serial IO a, Show a) => Type a -> TestTree
test_monoid _ = testGroup "Test monoid laws" [SC.testProperty "identity" $ identity @a
                                           ,localOption (4 :: SmallCheckDepth) $ SC.testProperty "assoiativity" $ assosiativ @a]

test_monoid2 = testGroup "Test monoid laws" [QC.testProperty "identity" $ identity @[Char]
                                           , QC.testProperty "assoiativity" $ assosiativ @[Char]]

main = defaultMain (test_monoid (T :: Type [Char]))
main2 = defaultMain test_monoid2

class AdditiveMonoid a where
  (+) :: a -> a -> a
  zero :: a

newtype Add a = Add a

instance (AdditiveMonoid m) => Monoid (Add m) where
  op (Add x) (Add y) = Add (x + y)
  e = Add zero




-- instance (AdditiveMonoid m) => Monoid m where
--   op = (+)
--   e = zero




class MultiplicativeMonoid m where
  (*) :: m -> m -> m
  one :: m

class (MultiplicativeMonoid m) => CommutativMonoid m where

class (MultiplicativeMonoid g) => Group g where
  (^-) :: g -> g

class (AdditiveMonoid a) => AbelianGroup a where
  negate :: a -> a

  (-) :: a -> a -> a
  x - y = x + (negate y)

class (AbelianGroup r, MultiplicativeMonoid r) => Ring r where

class (AbelianGroup r, CommutativMonoid r) => CommutativeRing r where

class (Ring k) => Field k where
  -- invers for all nonzero elements

class (Ring l, AbelianGroup m) => LeftModule l m where
  (*>) :: l -> m -> m

class (Ring r, AbelianGroup m) => RigthModule r m where
  (<*) :: m -> r -> m

-- class (LeftModule r m, RigthModule r m) => Module r m where
-- instance (CommutativeRing r, AbelianGroup m, LeftModule r m) => Module r m
-- instance (CommutativeRing r, AbelianGroup m, RigthModule r m) => Module r m
