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


t1 = Root blue [Root black [Root black []], Root red [], Root green []]
t2 = Root blue [Root red [], Root black [Root black []], Root green []]
t3 = Root blue [Root red [], Root green [], Root black [Root black []]]

out = draw (lcomb $ map (\(x,y) -> (NP x,y)) [(t1,1),(t2,6),(t3,1)]) "out.svg"

out2 = draw (graft_ [NP $Root blue []] [NP $ Root black [Root black [], Root black []]]) "out.svg"
