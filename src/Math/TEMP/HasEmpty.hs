{-# LANGUAGE
    NoImplicitPrelude
#-}

module Math.TEMP.HasEmpty where

import GHC.Base (Eq (..), Bool (..), otherwise)

class (Eq e) => HasEmpty e where
  empty :: e
  isEmpty :: e -> Bool
  isEmpty x
    | x == empty = True
    | otherwise = False



instance (Eq a) => HasEmpty [a] where
  empty = []
