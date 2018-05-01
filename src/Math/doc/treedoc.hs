{-# LANGUAGE
    NoImplicitPrelude
#-}

import Math.Combinatorics.Tree
import Math.Combinatorics.Tree.Operations
import Graphics
import Graphics.Colour
import Math.Algebra.Module
import Math.Algebra.Module.FreeModule
import Math.Algebra.Monoid
import Math.Algebra.Ring
import Math.Algebra.Group
import GHC.Base (($), fmap)
import Data.List (map)

-- Make some planar trees
t1 = R blue [R black [R black []], R red [], R green []]
t2 = R blue [R red [], R black [R black []], R green []]
t3 = R blue [R red [], R green [], R black [R black []]]

-- Draw the trees
dt1 = draw t1 "out.svg"
dt2 = draw t2 "out.svg"
dt3 = draw t3 "out.svg"


-- Make the trees non-planar and add them together.
out = draw (lcomb $ map (\(x,y) -> (NP x,y)) [(t1,1),(t2,6),(t3,1)]) "out.svg"

-- graft_ is grafting on basis elements in a module.
out2 = draw (graft_ [NP $R blue []] [NP $ R black [R black [], R black []]]) "out.svg"

out3 = draw (graft_ [R blue []] [R black [R black [], R black []]]) "out.svg"
