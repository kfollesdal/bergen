module Math.VectorSpace where

import Math.Field
import Math.Module

class (Field k, Module k b) => VectorSpace k b where


