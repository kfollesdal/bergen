{-# LANGUAGE
    TypeApplications,
    ScopedTypeVariables,
    FlexibleContexts
#-}
--     NoImplicitPrelude,

{-
Module      : Math.MonoidTest
Description : Test for monoid implementations. 
Maintainer  : kfo021@uib.no

Test for monoid implementations. 
-}

module Math.MonoidTest where

import Prelude hiding ((*))

import Math.Monoid

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.SmallCheck.Series (Serial)

import Math.Properties.Elements (unit)
import Math.Properties.Operations (assosiative)



data Type a = Test

monoid :: forall a . (Eq a, Serial IO a, Show a) => Type a -> (a -> a -> a) -> a -> [TestTree]
monoid _ op u = [SC.testProperty "unit laws" $ unit @a op u
                ,SC.testProperty "assoiativity law" $ assosiative @a op]
-- ,localOption (4 :: SmallCheckDepth) $

monoidQC :: forall a . (Eq a, Arbitrary a, Show a)=> Type a -> (a -> a -> a) -> a -> [TestTree]
monoidQC _ op u = [QC.testProperty "unit laws" $ unit @a op u
                  ,QC.testProperty "assoiativity law" $ assosiative @a op]

test_Math_Monoid = [testGroup "Monoid (Integer,*,1) [SmallCheck]" $ monoid (Test :: Type Integer) (*) u
                   ,testGroup "Monoid (Integer,*,1) [QuickCheck]" $ monoidQC (Test :: Type Integer) (*) u
                   ,testGroup "Monoid (Int,*,1) [SmallCheck]" $ monoid (Test :: Type Int)  (*) u
                   ,testGroup "Monoid (Int,*,1) [QuickCheck]" $ monoidQC (Test :: Type Int)  (*) u]

