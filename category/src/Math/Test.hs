{-# LANGUAGE
  NoImplicitPrelude,
  FlexibleInstances,
  MultiParamTypeClasses,
  TypeFamilies,
  GADTs,
  StandaloneDeriving,
  FlexibleContexts,
  RankNTypes
#-}

module Math.Test where

import Math.Monoid
import Math.Group
import Math.Module
import Math.Ring
import Math.Algebra
import Math.CoAlgebra
import GHC.Base hiding (Module, (*>))
import Data.List ((++), map, head)
import Data.Bool
import Data.Char
import Data.String
import Text.Show
import Data.Eq
import GHC.Num (Integer)
import GHC.Classes (Ord)

import Data.Category.Limit

import Math.Bialgebra
import Math.Module.FreeModule
-- import Math.Algebras.VectorSpace (Vect (..), zerov, add, negatev,smultL)

algebraFM :: AlgebraD (FreeM Integer String)
algebraFM = AlgebraD {
  unitD = \_ -> zerov,
  multD = \(FM xs) -> FM [(x ++ y,k) | ((x,y),k) <-xs]
                     }

data CoAlg = CoAlg

instance CoAlgebra CoAlg  (FreeM Integer String) where
  counit _ = \(FM xs) -> 1
  comult _ = \(FM xs) -> FM [((x,x),k) | (x,k) <- xs]

data Alg = Alg

instance Algebra Alg (FreeM Integer String) where
  unit _ = \_ -> zerov
  mult _ = \(FM xs) -> FM [(x ++ y,k) | ((x,y),k) <-xs]

bialgebra :: Bialgebra Alg CoAlg (FreeM Integer String)
bialgebra = Bialgebra {
  algebra = Alg,
  coalgebra = CoAlg
                      }

----------------------------

type family Mod name :: *

class (Module2 (Mod name)) => Algebra2 name where
  unit2 :: name -> Scalar (Mod name) -> (Mod name)
  mult2 :: name -> Tensor (Mod name) (Mod name) -> (Mod name)


class (Module2 (Mod name)) => CoAlgebra2 name where
  counit2 :: name -> Mod name -> Scalar (Mod name)
  comult2 :: name -> Mod name -> Tensor (Mod name) (Mod name)

data Bialgebra2 alg coalg = Bialgebra2 {
  algebra2 :: (Algebra2 alg) => alg,
  coalgebra2 :: (CoAlgebra2 coalg) => coalg
  }

bialgebra2 :: Bialgebra2 Alg CoAlg
bialgebra2 = Bialgebra2 {
      algebra2 = Alg,
      coalgebra2 = CoAlg
                        }


type instance Mod Alg = FreeM Integer String
type instance Mod CoAlg = FreeM Integer String
type instance Mod (Bialgebra2 alg coalg) = Mod alg

instance (Algebra2 alg) => Algebra2 (Bialgebra2 alg coalg) where
  unit2 name = unit2 (algebra2 name)
  mult2 name = mult2 (algebra2 name)

instance Algebra2 Alg where
  unit2 _ = \_ -> zerov
  mult2 _ = \(FM xs) -> FM [(x ++ y,k) | ((x,y),k) <-xs]

instance CoAlgebra2 CoAlg where
  counit2 _ = \(FM xs) -> 1
  comult2 _ = \(FM xs) -> FM [((x,x),k) | (x,k) <- xs]
