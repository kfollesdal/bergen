{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DataKinds, ConstraintKinds, UndecidableInstances #-}

import Data.Bool

class Collection f where
  type Element f :: *
  singelton :: Element f -> f
  con_ :: f -> f -> f

instance Collection [a] where
  type Element [a] = a
  singelton x = [x]
  con_ xs ys = xs ++ ys

con1 :: (Collection f) => Element f -> Element f -> f
con1 x y = con_ (singelton x) (singelton y)

con2 :: (Collection f) => Element f -> f -> f
con2 x y = con_ (singelton x) y

con3 :: (Collection f) => f -> Element f -> f
con3 x y = con_ x (singelton y)

con4 :: (Collection f) => f -> f -> f
con4 = con_

data Type = ZeroZero | ZeroOne | OneZero | OneOne

type family Sel a b f :: Type where
  Sel f f f = ZeroZero
  Sel a a f = OneOne
  Sel a f f = OneZero
  Sel f a f = ZeroOne

class (t~Sel a b f) => Concat' t a b f where
  con' :: a -> b -> f

instance (a ~ f, b ~ f, Collection f) => Concat' ZeroZero a b f where
  con' = con_

instance (Sel a b f ~ OneOne, a ~ b, a ~ Element f, b ~ Element f, Collection f) => Concat' OneOne a b f where
  con' x y = con_ (singelton x) (singelton y)

instance (Sel a b f ~ OneZero, a ~ Element f, b ~ f, Collection f) => Concat' OneZero a b f where
  con' x ys = con_ (singelton x) ys

instance (Sel a b f ~ ZeroOne, a ~ f, b ~ Element f, Collection f) => Concat' ZeroOne a b f where
  con' xs y = con_ xs (singelton y)

con :: (Concat' (Sel a b f) a b f) => a -> b -> f
con = con'

-- class (Collection f) => Lift a f where
--   lift :: a -> f
--
-- instance (Collection f, f ~ g) => Lift f g where
--   lift = id
--
-- instance (Collection f, x ~ Element f) => Lift x f where
--   lift x = singelton x

-- con :: (Lift a x, Lift b x) => a -> b -> [x]
-- con










-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
-- {-# LANGUAGE DataKinds, ConstraintKinds #-}
--
-- import Maht.Module.FreeModule
--
-- newtype F k a = B a | F (FreeModule k a)
-- class Collection f where
--   type Element f :: *
--   insert :: Element f -> f
--   add_ :: f -> f -> f
--
-- instance Collection [a] where
--   type Element [a] = a
--   insert x = [x]
--   add_ xs ys = xs ++ ys
--
-- data OneOne
-- data OneZero
-- data ZeroOne
-- data ZeroZero
--
-- type family Sel a b f where
--   Sel a a f = OneOne
--   Sel a f f = OneZero
--   Sel f a f = ZeroOne
--   -- Sel f f f = ZeroZero
--
-- class (t ~ Sel a b f) => Concat' t a b f where
--   con' :: a -> b -> f
--
-- instance (Collection f, a ~ Element f, b ~ Element f, Sel a a [a] ~ OneOne) => Concat' OneOne a b f where
--   con' x y = add_ (insert x) (insert y)
--
-- -- instance (Sel a [a] [a] ~ OneZero) => Concat' OneZero a [a] [a] where
-- --   con' x ys = add_ (insert x) ys
-- --
-- -- instance (Sel [a] a [a] ~ ZeroOne) => Concat' ZeroOne [a] a [a] where
-- --   con' xs y = add_ xs (insert y)
-- --
-- -- instance (Sel [a] [a] [a] ~ ZeroZero) => Concat' ZeroZero [a] [a] [a] where
-- --   con' xs ys = add_ xs ys
--
-- type Concat a b f = (Concat' (Sel a b f) a b f)
--
-- con :: (Concat a b f) => a -> b -> f
-- con = con'
-- --
-- -- instance (Collection f, a ~ Element f) => Concat a f f where
-- --   con = con10
-- --
-- -- instance (Collection f, a ~ Element f) => Concat f a f where
-- --   con = con01
-- --
-- -- test10 :: [String]
-- -- test10 = con "a" ["b","c"]
-- --
-- -- test01 :: [String]
-- -- test01 = con ["b","c"] "a"
--
--
--
--
--
-- --
-- -- con10 :: (Collection f, a ~ Element f) => a -> f -> f
-- -- con10 = con' (undefined::OneZero)
-- --
-- -- con01 :: (Collection f, a ~ Element f) => f -> a -> f
-- -- con01 = con' (undefined::ZeroOne)
-- --
-- -- con00 :: (Collection f) => f -> f -> f
-- -- con00 = con' (undefined::ZeroZero)
-- --
-- -- con11 :: (Collection f, a~Element f, b~Element f) => a -> b -> f
-- -- con11 = con' (undefined::OneOne)
