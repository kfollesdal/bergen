{-# LANGUAGE

  TypeFamilies,
  ViewPatterns
#-}
--  NoImplicitPrelude,
module Math.Tree where

isSix :: Int -> Bool
isSix x = x == 6

test :: Int -> String
test (isSix -> True) = "Yes, the number is"

-- class Forrest f where
--   type Tree a :: *
--   bPlus :: Forrest a -> Tree a
--   bMinus :: Tree a -> Forrest a

graft_,(⊵) :: Forrest a -> Forrest a -> FreeModule k
graft_ u (mkSplit -> MK c l r) = case isTree u of
  True -> mk c (u ⊵ l) (lift r) + mk c (lift l) (lift (u ̇ r) + u ⊵ r)
  False -> (lift)
