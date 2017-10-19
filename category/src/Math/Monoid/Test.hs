{-# LANGUAGE
    TypeApplications,
    ScopedTypeVariables,
    FlexibleContexts
#-}

{-
Module      : Math.Monoid.Test
Description : Test for monoid implementations. 
Maintainer  : kfollesdal@gmail.com

Test for monoid implementations. 
-}

module Math.Monoid.Test where

import Prelude hiding ((*))

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.SmallCheck.Series (Serial)

import Math.Monoid
import Math.Properties.Elements (unit)
import Math.Properties.Operations (assosiative)



data Type a = Test

test_monoid :: forall a . (Eq a, Serial IO a, Show a) => Type a -> (a -> a -> a) -> a -> TestTree
test_monoid _ op u = testGroup "Test monoid laws" [SC.testProperty "unit laws" $ unit @a op u
                                           ,SC.testProperty "assoiativity law" $ assosiative @a op]
-- ,localOption (4 :: SmallCheckDepth) $

test_monoidQC :: forall a . (Eq a, Arbitrary a, Show a)=> Type a -> (a -> a -> a) -> a -> TestTree
test_monoidQC _ op u = testGroup "Test monoid laws" [QC.testProperty "unit laws" $ unit @a op u
                                           ,QC.testProperty "assoiativity law" $ assosiative @a op]

test = defaultMain (test_monoid (Test :: Type Integer) (*) u)
test2 = defaultMain (test_monoid (Test :: Type Int)  (*) u)

test3 = defaultMain (test_monoidQC (Test :: Type Integer) (*) u)
test4 = defaultMain (test_monoidQC (Test :: Type Int)  (*) u)
