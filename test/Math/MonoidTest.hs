{-# LANGUAGE
    TypeApplications,
    ScopedTypeVariables,
    FlexibleContexts
#-}
--     NoImplicitPrelude,

{-
Module      : Math.MonoidTest
Description : Test for monoid implementations.
Maintainer  : kfollesdal@gmail.com

Test for monoid implementations.
-}

module Math.MonoidTest where

import Prelude hiding ((*))

import Math.Algebra.Monoid

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.SmallCheck.Series (Serial)

import Math.Algebra.Properties.Elements (unit)
import Math.Algebra.Properties.Operations (assosiative, commutative)

data Type a = Test


-- Test of monoids
test_monoids :: TestTree
test_monoids = testGroup "Monoids" [testGroup "([Char],++,[])" $ monoidQC (Test :: Type [Char]) (++) []]

monoidSC :: forall a . (Eq a, Serial IO a, Show a) => Type a -> (a -> a -> a) -> a -> [TestTree]
monoidSC _ op u = [SC.testProperty "unit laws" $ unit @a op u
                ,SC.testProperty "assoiativity law" $ assosiative @a op]
-- ,localOption (4 :: SmallCheckDepth) $

monoidQC :: forall a . (Eq a, Arbitrary a, Show a)=> Type a -> (a -> a -> a) -> a -> [TestTree]
monoidQC _ op u = [QC.testProperty "unit laws" $ unit @a op u
                  ,QC.testProperty "assoiativity law" $ assosiative @a op]


-- Test of commutative Monoids
test_commutativMonoid :: TestTree
test_commutativMonoid = testGroup "Commutative Monoids" [scCommutativeMonoid, qcCommutativeMonoid]

scCommutativeMonoid :: TestTree
scCommutativeMonoid = testGroup "[SmallCheck]"
  [testGroup "(Integer,*,1)" $ commutativeMonoidSC (Test :: Type Integer) (*) u,
   testGroup "(Int,*,1)" $ commutativeMonoidSC (Test :: Type Int) (*) u
  ]

qcCommutativeMonoid :: TestTree
qcCommutativeMonoid = testGroup "[QuickCheck]"
  [testGroup "(Integer,*,1)" $ commutativeMonoidQC (Test :: Type Integer) (*) u,
   testGroup "(Int,*,1)" $ commutativeMonoidQC (Test :: Type Int) (*) u
  ]

commutativeMonoidSC :: forall a . (Eq a, Serial IO a, Show a) => Type a -> (a -> a -> a) -> a -> [TestTree]
commutativeMonoidSC t op u = [testGroup "Monoid" $ monoidSC t op u,
                            SC.testProperty "commutative law" $ commutative @a op]

commutativeMonoidQC :: forall a . (Eq a, Arbitrary a, Show a) => Type a -> (a -> a -> a) -> a -> [TestTree]
commutativeMonoidQC t op u = [testGroup "Monoid" $ monoidQC t op u,
                              QC.testProperty "commutative law" $ commutative @a op]
