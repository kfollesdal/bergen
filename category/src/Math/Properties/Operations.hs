module Math.Properties.Operations (
  -- ***
  -- $asso
  assosiative,
  distributive
  )
where

-- $asso
-- __Definition:__ A binary operation \(\bullet\) is /assosiative/ if \(x \bullet (y \bullet z) = (x \bullet y) \bullet z\).


assosiative :: (Eq m) => (m -> m -> m) -> m -> m -> m -> Bool
assosiative op x y z = x `op` (y `op` z) == (x `op` y) `op` z

left_distributive :: (Eq m) => (m -> m -> m) -> (m -> m -> m) -> m -> m -> m -> Bool
left_distributive op1 op2 x y z = x `op1` (y `op2` z) == (x `op1` y) `op2` (x `op1` z)

right_distributive :: (Eq m) => (m -> m -> m) -> (m -> m -> m) -> m -> m -> m -> Bool
right_distributive op1 op2 x y z = (x `op2` y) `op1` z == (x `op1` z) `op2` (y `op1` z)

distributive :: (Eq m) => (m -> m -> m) -> (m -> m -> m) -> m -> m -> m -> Bool
distributive op1 op2 x y z = left_distributive op1 op2 x y z && right_distributive op1 op2 x y z
