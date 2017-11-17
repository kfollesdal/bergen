{-# LANGUAGE
  NoImplicitPrelude,
  TypeApplications,
  ScopedTypeVariables
#-}

import Test.LeanCheck
import Test.LeanCheck.Function.ListsOfPairs
import Math.Algebra.Properties.Elements
import GHC.Base (Int (..), Bool (..), IO (..), Eq (..), ($))
import Prelude (uncurry)
import Control.Monad (mapM_)
import GHC.Show (Show (..))
import Math.Algebra.Monoid
import Math.Algebra.Properties.Operations

data Mk a = I | Mk a (Mk a) (Mk a) deriving (Show)

data A = A deriving (Show)

instance Listable A where
  tiers = cons0 A

instance Listable a => Listable (Mk a) where
  tiers = cons3 Mk
       \/ cons0 I

data Type' a = Test'

-- test :: forall a . (Listable a, AddidtativeMonoid a, Show a, Eq a) => Type' a -> IO()
-- test _ = mapM_ check [unit @a (+) zero, assosiative @(a->a) (+)]

testf :: forall a . (Listable a, AddidtativeMonoid a, Show a, Eq a) => Type' a -> IO()
testf _ = check (assosiative @a (+))

testInt = testf (Test' :: Type' Int)
