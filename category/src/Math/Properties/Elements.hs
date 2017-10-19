{-|
Module      : Math.Properties.Elements
Description : Properties for elements in algebraic structures.
Maintainer  : Kristoffer K. Føllesdal <kfollesdal@gmail.com>

Properties for elements in algebraic structures.
-}


module Math.Properties.Elements (
  -- ** Unit laws
  -- | __Definition:__ Given a binary operation \(\bullet : A \times A \to A\),
  -- a element \(u \in A\) is a /left unit/ for the operation if 
  -- \(u \bullet x = x \), /rigth unit/ if \(x \bullet u = x\) and a u is  a
  -- /unit/ for \(\bullet\) if it is both a left and rigth unit. 
  
  left_unit,
  right_unit,
  unit
                                ) where

left_unit :: (Eq m) => (m -> m -> m) -> m -> m -> Bool
left_unit op e x = e `op` x == x

right_unit :: (Eq m) => (m -> m -> m) -> m -> m -> Bool
right_unit op e x = x `op` e == x

unit :: (Eq m) => (m -> m -> m) -> m -> m -> Bool
unit op e x = left_unit op e x && right_unit op e x
