{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, ViewPatterns #-}

module Graphics --(Drawable(..), Colors, draw', draw'', module Diagrams.Prelude, Diagrams.Backend.SVG.B, center0markX
               --, mkDiagramVect, markOrigin, moveOriginBack, stdOptsVect, OptsVect (..))
       where

import qualified Data.Tree (Tree (Node), Forest)
-- import VectorSpace (Vect(..),terms, Tensor)
-- import Tree.OrderedForest (OF(..), strRep)
-- import Data.Ratio (Ratio, denominator, numerator)

import Math.Combinatorics.Tree (PlanarTree (..), NonPlanarTree, planar)
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree (symmLayout', slHSep, slVSep, renderTree)
import Diagrams.Backend.SVG (B, renderSVG)
import Math.Algebra.Module.FreeModule (FModule (..))
import Math.Algebra.Module (FreeModule (..))
import Math.Algebra.Ring

toHaskellTree :: PlanarTree n -> Data.Tree.Tree n
toHaskellTree (R n xs) = Data.Tree.Node n (map toHaskellTree xs)

type Colors = Colour Double

instance Ord Colors where
  compare x y =
    let xr = toSRGB x
        yr = toSRGB y in
    case compare (channelRed xr) (channelRed yr) of
    LT -> LT
    GT -> GT
    EQ -> case compare (channelGreen xr) (channelGreen yr) of
      LT -> LT
      GT -> GT
      EQ -> compare (channelBlue xr) (channelBlue yr)

draw'' :: Diagram B -> Double -> Diagram B -> String -> IO()
draw'' unitDiagrm h_unitDiagrm_out object filePath =
  renderSVG filePath (mkHeight (h_unitDiagrm_out*ratio)) object
  where
    h_unitDiagrm_local = height unitDiagrm
    h_diagrm_local = height object
    ratio = h_diagrm_local/h_unitDiagrm_local

class Drawable a where
  mkDiagram :: a -> Diagram B
  draw :: a -> String -> IO()
  draw x = draw' (mkDiagram x)

draw' :: Diagram B -> String -> IO()
draw' = draw'' (mkDiagram (Data.Tree.Node "*" [Data.Tree.Node "*" []])) 30

instance Drawable (PlanarTree n) where
  mkDiagram t = mkDiagram (toHaskellTree t)

instance Drawable (NonPlanarTree n) where
  mkDiagram = mkDiagram . planar

instance Drawable (Data.Tree.Tree n) where
  mkDiagram t =
    renderTree (\x -> circle (1/6) # fc black #lw 0 #named x)
               (~~)
               (symmLayout' (with & slHSep .~ 1 & slVSep .~ 1) (nameRoot t))
    #frame 0.02 # reflectY # lwL 0.07 #lc gray -- Do we need to use frame?
    where
      nameRoot (Data.Tree.Node _ ts)  = Data.Tree.Node (0::Int) (map (fmap (const (1::Int))) ts)

instance {-# INCOHERENT #-} Drawable (PlanarTree Colors) where
  mkDiagram t = mkDiagram (toHaskellTree t)

instance {-# INCOHERENT #-} Drawable (NonPlanarTree Colors) where
  mkDiagram = mkDiagram . planar

instance {-# OVERLAPPING #-} Drawable (Data.Tree.Tree Colors) where
  mkDiagram t =
    renderTree (\(color,x) -> if color == white then
                                circle (1/6-0.01) # fc color #lwL 0.01 #lc black #named x
                              else circle (1/6) # fc color #lw 0 #named x
               )
               (~~)
               (symmLayout' (with & slHSep .~ 1 & slVSep .~ 1) (nameRoot t))
    #frame 0.02 # reflectY # lwL 0.07 #lc gray -- Do we need to use frame?
    where
      nameRoot (Data.Tree.Node r ts) = Data.Tree.Node (r, 0::Int) (map (fmap (\n -> (n,1::Int))) ts)

instance (Drawable (PlanarTree n)) => Drawable [PlanarTree n] where
  mkDiagram [] = text "\120793" #moveOriginBy (0.6*unit_Y)
                 <> phantom (circle (1/6) #lw 0 :: Diagram B) #named (0::Int) #alignB
                 <> phantom (rect 0.6 1.2 #lw 0 :: Diagram B) #alignB
  mkDiagram [x] = mkDiagram x
  mkDiagram f = hcat (map mkDiagram f) #center0markX

instance {-# INCOHERENT #-} (Drawable (NonPlanarTree Colors)) => Drawable [NonPlanarTree Colors] where
  mkDiagram = mkDiagram . planar

instance (Drawable (NonPlanarTree n)) => Drawable [NonPlanarTree n] where
  mkDiagram = mkDiagram . planar
--
-- instance Drawable (OF a) where
--   mkDiagram f = mkDiagram (strRep f)
--
-- instance {-# OVERLAPPING #-} Drawable (OF Colors) where -- Is it possible to avoid this instance?
--   mkDiagram f = mkDiagram (strRep f)
--
data OptsVect k = OptsVect {
  _mkCof :: k -> Diagram B,
  _mkSign :: String -> Diagram B,
  _cofPosition :: (Double,Double),
  _useMark :: Bool,
  _cofPosition' :: Diagram B -> Diagram B -> Diagram B}

stdOptsVect :: (Eq k, Num k, Show k) => OptsVect k
stdOptsVect = OptsVect {
  _mkCof = _mkCof_,
  _mkSign = _mkSign_,
  _cofPosition = _cofPosition_ ,
  _useMark = True,
  _cofPosition' = beneath}
  where
    _mkCof_ c
      | abs c == 1 = strutY 0.6
      | otherwise = atop (strutY 0.6) (text(show (abs c)) #fontSize(local 0.6))
    _mkSign_ s = strutY 1.8 <> strut 0.6 <> text s #fontSize (local 0.6) #moveOriginBy (0.6*unit_Y)
    _cofPosition_ = (0,0.6)


mkDiagramFM :: (Num k, Ord k, Drawable b, Ord b, CommutativeRing k) => OptsVect k -> FModule k b -> Diagram B
mkDiagramFM _ (null.decompose -> True) = text "0" #fontSize(local 0.6) <> phantom (rect 0.6 1.2 :: Diagram B)
mkDiagramFM (OptsVect mkCof mkSign cofPosition useMark cofPosition') v = hcat (concat (first (head (decode v)) : map rest (tail (decode v))))
    where decode x = map decode' (decompose x)
            where decode' (b, c) = if c < 0  then ("-", addCof b c)
                                   else ("+", addCof b c)
          addCof b c = case lookupName (0::Int) (mkDiagram b) of
            Nothing -> cofPosition' (mkCof c) (mkDiagram b)
            Just sub -> if useMark
                        then
                          atop (place (mkCof c) (location sub .+^ (fst cofPosition*^unit_X) .+^ (snd cofPosition*^unit_Y))) (mkDiagram b)
                        else
                          cofPosition' (mkCof c) (mkDiagram b)
          first (s, dia)
            | s == "-" = rest (s,dia)
            | otherwise = [mempty,dia]
          rest (s, dia) = [mkSign s, dia]

instance {-# OVERLAPPABLE #-}(Num k, Eq k, Ord k, Show k, Drawable b, Ord b, CommutativeRing k) => Drawable (FModule k b) where
  mkDiagram = mkDiagramFM stdOptsVect

-- instance {-# INCOHERENT #-}(Num k, Eq k, Ord k, Show k, Drawable b, Integral k) => Drawable (Vect (Ratio k) b) where
--   mkDiagram = mkDiagramVect (stdOptsVect {_cofPosition = (0,1),_mkCof = _mkCof_}) where
--     _mkCof_ x = let t = abs $ numerator x
--                     n = abs $ denominator x
--                 in
--                   case n of
--                     1 -> if t == 1 then
--                            mempty
--                          else
--                            text(show t) #fontSize(local 0.6)
--                     _ -> text(show t ++ "/" ++ show n) #fontSize(local 0.6)
--
-- instance (Drawable a, Drawable b) => Drawable (Tensor a b) where
--   mkDiagram x = hcat [mkDiagram (fst x)
--             , strutY 1.8 <> strut 0.6 <> (text "\8855" #fontSize (local 0.6) #moveOriginBy (0.6*unit_Y) )
--             , mkDiagram (snd x)] # center0markX
--
center0markX :: Diagram B -> Diagram B
center0markX dia = case lookupName (0::Int) dia of
                     Nothing -> dia
                     Just sub -> atop
                                 (place
                                  (phantom (circle (1/6) #lw 0:: Diagram B) #named (0::Int))
                                  (p2 (xcordCenter dia, snd (unp2 (location sub)))))
                                 (localize dia)

xcordCenter :: (Enveloped a, V a ~ V2) => a -> N a
xcordCenter dia = fst (unp2
                       (lerp 0.5
                        (envelopeBoundary unitX dia)
                        (envelopeBoundary (negated unitX) dia)
                       )
                      )

-- markOrigin x = mempty #named 'o' <> x
--
-- moveOriginBack x = case lookupName 'o' x of
--   Nothing -> x
--   Just sub -> x #moveOriginTo (location sub)
