import Test.LeanCheck

data Mk a = I | Mk a (Mk a) (Mk a) deriving (Show)

data A = A deriving (Show)

instance Listable A where
  tiers = cons0 A

instance Listable a => Listable (Mk a) where
  tiers = cons3 Mk
       \/ cons0 I
