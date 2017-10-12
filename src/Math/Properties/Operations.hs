{-|
Module      : Math.Properties.Operations
Description : Properties for operations on algebraic structures.
Maintainer  : Kristoffer K. Føllesdal <kfo021@uib.no>

Properties for operations on algebraic structures.
-}

module Math.Properties.Operations (
  -- ** Assosiative law
  -- $asso
  assosiative,

  -- ** Distributive law
  -- $distributive
  left_distributive,
  right_distributive,
  distributive,

  -- ** Commutative law
  -- $commutative
  commutative
  )
where

-- $asso
-- __Definition:__ A binary operation \(\bullet\) is /assosiative/ if \(x \bullet (y \bullet z) = (x \bullet y) \bullet z\).

assosiative :: (Eq m) => (m -> m -> m) -> m -> m -> m -> Bool
assosiative op x y z = x `op` (y `op` z) == (x `op` y) `op` z

-- $distributive
-- __Definition:__ A binary operation \(\bullet\) is /left distributive/ over a binary operation \(+\) if \(x \bullet (y+z) = x \bullet y + x \bullet z\),
-- /rigth distributive/ if \((x + y) \bullet z = x \bullet z + y \bullet z\) and /distributive/ over \(+\) if both left and right distributive.

left_distributive :: (Eq m) => (m -> m -> m) -> (m -> m -> m) -> m -> m -> m -> Bool
left_distributive op1 op2 x y z = x `op1` (y `op2` z) == (x `op1` y) `op2` (x `op1` z)

right_distributive :: (Eq m) => (m -> m -> m) -> (m -> m -> m) -> m -> m -> m -> Bool
right_distributive op1 op2 x y z = (x `op2` y) `op1` z == (x `op1` z) `op2` (y `op1` z)

distributive :: (Eq m) => (m -> m -> m) -> (m -> m -> m) -> m -> m -> m -> Bool
distributive op1 op2 x y z = left_distributive op1 op2 x y z && right_distributive op1 op2 x y z

-- $commutative
-- __Definition:__ A binary operation \(\bullet\) is /commutative/ if \(x \bullet y = y \bullet x \).

commutative :: (Eq m) => (m -> m -> m) -> m -> m -> Bool
commutative op x y = x `op` y == y `op` x
