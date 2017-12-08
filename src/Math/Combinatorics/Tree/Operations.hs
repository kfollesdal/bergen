module Math.Combinatorics.Tree.Operations where

graft_, (⊵) :: (FreeModule m, Fo t) => t -> t -> m t
graft_ (isEmpty -> True) w = i w
graft_ v (isEmpty -> True) = zero
graft_ t@(isTree -> True) (mkSplit -> (c, l, r)) =
  mk c (t ⊵ l) (i r) + mk c (i l) (i (u⋅r) + t ⊵ r)
graft_ (headTail -> (t,f)) w = (i t) ▷ (f ⊵ w) - (t ⊵ f) ▷ (i w)

  class GraftingProduct f where
    graft_, (⊵) :: f → f → FreeModule k f
    graft :: (FreeModule k f) ⨂ (FreeModule k f) → FreeModule k f
    graft = tensorBilinear graft_
    graft', (▷) :: FreeModule k f → FreeModule k f → FreeModule k f
    graft' = bilinear graft_

  instance (Forrest f) ⇒ GraftingProduct f where
    graft_ (isEmpty -> True) w = lift w
    graft_ w (isEmpty -> True) = zero
    graft_ t@(isTree -> True) (mkSplit -> (c, l, r)) =
      mk c (t ⊵ l) (lift r) + mk c (lift l) (lift (u⋅r) + t ⊵ r)
    graft_ (headTail -> (t,f)) w = (lift t) ▷ (f ⊵ w) - (t ⊵ f) ▷ (lift w)
