{-# LANGUAGE
  NoImplicitPrelude,
  TypeFamilies,
  ConstraintKinds,
  MultiParamTypeClasses,
  UndecidableInstances,
  UndecidableSuperClasses
#-}

-- TODO
-- @ Better name on GeneratorType?
-- * HasBasis / FreeObject combine together

module Math.TEMP.FreeMonoid where

import Math.Algebra.Monoid
import GHC.Exts (Constraint)

class Free m where
  type GeneratorType m :: *
  --type Category m :: * -> Constraint
  i :: k (GeneratorType m) m
  extend :: (GeneratorType m -> n) -> (m -> n)
  -- Må sørge for at n er samme "class"/ i samme ketegori som m.

class (MultiplicativeMonoid m, Free m) => FreeMonoid m where
  head :: m -> GeneratorType m
  tail :: m -> m
  headTail :: m -> (GeneratorType m, m)
  headTail x = (head x, tail x)
