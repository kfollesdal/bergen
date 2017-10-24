{-# LANGUAGE
  NoImplicitPrelude,
  UnicodeSyntax,
  TypeFamilies,
  ViewPatterns
#-}

-- FreeModule Monad
-- change name basis to lift

module Math.Tree where

import GHC.Base (Eq (..), Ord (..), Bool (..), error, ($),(++))
import Text.Show (Show(..))
import Math.Module
import Math.Module.FreeModule

data MK c = I | MK {root:: c, left:: MK c, rigth :: MK c} deriving (Eq, Ord, Show)

instance Forrest MK where
  type Tree MK c = MK c
  isTree I = False
  isTree (MK c I x) = True
  isTree (MK c x y) = False
  isEmpty I = True
  isEmpty x = False
  bPlus c x = MK c I x
  bMinus (MK c I f) = f
  bMinus x = error $ show x ++ "Is not a Tree"
  mk_ c l r = MK c l r
  mkSplit (MK c l r) = (c,l,r)
  --unconcat I = (I,I)
  --unconcat (MK c l r) =

class Forrest f where
    type Tree f c ∷ *
    isTree ∷ f c -> Bool
    isEmpty ∷ f c -> Bool
    bPlus ∷ c -> f c → Tree f c
    bMinus ∷ (Show c) ⇒ Tree f c -> f c
    mk_ ∷ c -> f c -> f c -> f c
    --mk ∷ c -> FreeModule k (f c) → FreeModule k (f c) → FreeModule k (f c)

    mkSplit ∷ f c -> (c, f c,f c)
    --unconcat ∷ f c -> (Tree f c, f c)

-- class GraftingProduct f where
--   graft_, (⊵) :: f → f → FreeModule k f
--   graft :: (FreeModule k f) ⨂ (FreeModule k f) → FreeModule k f
--   graft = tensorBilinear graft_
--   graft', (▷) :: FreeModule k f → FreeModule k f → FreeModule k f
--   graft' = bilinear graft_
--
-- instance (Forrest f) ⇒ GraftingProduct f where
--   graft_ (isEmpty -> True) w = lift w
--   graft_ w (isEmpty -> True) = zero
--   graft_ t@(isTree -> True) (mkSplit -> (c, l, r)) =
--     mk c (t ⊵ l) (lift r) + mk c (lift l) (lift (u⋅r) + t ⊵ r)
--   graft_ (unconcat -> (t,f)) w = (lift t) ▷ (f ⊵ w) - (t ⊵ f) ▷ (lift w)
