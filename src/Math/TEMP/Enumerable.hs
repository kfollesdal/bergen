{-# LANGUAGE
    NoImplicitPrelude
#-}

module Math.TEMP.Enumerable where

import GHC.Base (Int)

class Enumerable e where
  count :: e -> Int
