moodule Math.Algebra.TensorAlgebra where

import Math.FreeModule

type TensorAlgebra k b = FreeModule k (TA b)

data TA a = TA Int [a] deriving (Eq,Ord)

concatenation ::
concatenation (TA n xs) (TA m ys) = TA (n+m) (xs++ys)

tensorAlgebra :: (CommutativeRing k) => AlgebraD (FreeM k (TA b))
tensorAlgebra = AlgebraD {
  unitD ,
  multD = linear (\(x,y) ->
}
