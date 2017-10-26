{-# LANGUAGE
  NoImplicitPrelude,
  UnicodeSyntax,
  TypeFamilies,
  ViewPatterns,
  FlexibleInstances,
  MultiParamTypeClasses
#-}

-- FreeModule Monad
-- change name basis to lift

module Math.Tree where

import GHC.Base (Eq (..), Ord (..), Bool (..), error, ($),(++), undefined, Int)
import Text.Show (Show(..))
import Math.Module
import Math.Module.FreeModule
import Data.Monoid

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
  mkSplit I = (undefined,I,I)
  concat I x = x
  concat x I = x
  concat (MK c l r) (MK c2 l2 r2) = MK c l r2
  empty = I
  lift x = x
  --unconcat I = (I,I)
  --unconcat (MK c l r) =


-- evenetuekt (Monoid (f c)) => Forrest f c
class Forrest f where -- Monad
    type Tree f c ∷ *
    isTree ∷ f c -> Bool
    isEmpty ∷ f c -> Bool
    bPlus ∷ c -> f c → Tree f c
    bMinus ∷ (Show c) ⇒ Tree f c -> f c
    mk_ ∷ c -> f c -> f c -> f c
    --mk_ c v u = concat v (bPlus c u)
    --mk ∷ c -> FreeModule k (f c) → FreeModule k (f c) → FreeModule k (f c)

    mkSplit ∷ f c -> (c, f c,f c)
    mkSplit x = (mkRoot x, mkLeft x, mkRigth x)
    mkRoot :: f c -> c
    mkRoot (mkSplit -> (c,_,_)) = c
    mkLeft :: f c -> f c
    mkLeft (mkSplit -> (_,l,_)) = l
    mkRigth :: f c -> f c
    mkRigth (mkSplit -> (_,_,r)) = r
    concat :: f c -> f c -> f c
    empty :: f c
    lift :: Tree f c -> f c
    --unconcat ∷ f c -> (Tree f c, f c)

class LeftConcat a b f where -- Can we use Module?
  lconcat :: a -> b -> f

instance (Forrest f) => LeftConcat (f c) (f c) (f c) where
  lconcat u v = concat u v

instance (Forrest f, u ~ Tree f c, v ~ Tree f c) => LeftConcat u v (f c) where
  lconcat t f = concat (lift t) (lift f)

instance (Forrest f, u ~ Tree f c) => LeftConcat u (f c) (f c) where
  lconcat t f = concat (lift t) f

instance (Forrest f, u ~ Tree f c) => LeftConcat (f c) u (f c) where
  lconcat f t = concat f (lift t)


instance (Forrest f) => Monoid (f c) where
  mempty = empty
  mappend = concat


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
