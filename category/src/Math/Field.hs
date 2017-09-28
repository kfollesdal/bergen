module Math.Field where

import Math.Ring

class (CommutativeRing k) => Field k where
  -- invers for all nonzero elements
  -- invers :: k -> k
